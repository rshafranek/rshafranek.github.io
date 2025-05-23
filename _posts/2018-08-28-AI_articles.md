---
title: "AI-Generated Political Science Article Names"
excerpt: "In advance of APSA 2018, I trained a neural network to generate political science article names. Watch out, human political scientists. "
date: 2018-08-28
classes: wide
header:
  teaser: "/images/ai_articles.jpg"
---

For fun and simply because I could (without stopping to ask whether I *should*), I trained a neural network to generate political science article names.

Specifically, I used a Python-based implementation of a recurrent neural network to generate new article names based on a novel dataset of 4,520 political science publications. This dataset includes every article published from 2008-2017 (excluding letters, editorials, and book reviews) in the following journals: *American Journal of Political Science, American Political Science Review, British Journal of Political Science, Comparative Politics, International Organization, International Theory, Journal of Experimental Political Science, Journal of Political Philosophy, Journal of Politics, Political Analysis, Political Science Research and Methods, Public Opinion Quarterly, Studies in American Political Development, World Politics*.

Thanks to Max Woolf's [textgenrnn](https://github.com/minimaxir/textgenrnn/) package, the process couldn't be more straightforward. We simply load the text file containing our training data, and set the neural net to studying: 

```python
# import the package
from textgenrnn import textgenrnn

# train the neural net
t = textgenrnn()
t.train_from_file('articles.txt', num_epochs=10)

# generate some article names
t.generate(10, temperature=0.66)
```

What’s the verdict? Are we ready for artificial intelligence to replace human political scientists? The first batch seems pretty on-the-mark:

>*The Conflicting of Partisan Political Parties*
>
>*The International Experiment on the Role of Democratic Representation*
>
>*The Politics of Political Participation in the USA*
>
>*The Political Content of International Conflict*
>
>*The Origins of Politics and the World Party Study*
>
>*International Responsibility and the Case of Robust Control, 1932-2000*

Seems like pretty plausible stuff. Throw in some articles about democratic peace, framing, maybe a little Kant, and you’ve got the greatest hits. Give the AI a little more leeway, though, and things get more interesting:

>*Candidate Data or Cash: Paternal Moral States*
>
>*Loyalism and the Mirth Politics of Candidate Identity*
>
>*Partisan Based Social Response Liquid Alternatives*
>
>*Disrobing the Logic of Pivotal Indian Attemporation*
>
>*Transfaction of Brandoo Political Action: Explaining the Electoral Logics of Classified Outcomes*
>
>*Moralized Government From Random Authoritarian Oranger Assolation*

Crank things up a little bit more, and things really start to go off the rails. (I’m intrigued by the appearance of the weapon-metaphors(?)).

>*ESPN and the European USATATiMophism of Children*
>
>*What is the World War Party*
>
>*Punishing Sword: A Financial Politics*
>
>*Motivated Rights and the Rocketification of Political Conflicts*

A few more fun ones:

>*The Distork: State Perspective on Campaign Preferences*
>
>*The Death Party Compliance: The Moderating Micro-Eaple Eurosity of the Quevel GOT*
>
>*Trumposition and Public Operability in Communist Theory*
>
>*Errorism and International Social Competition*
>
>*The Funching of Foreign Data: Party Strategies and Electoral Politics*
>
>*Moderating Political Ambition: The City of Santa influences of RometATATATATA*
>
>*A Private Stargoblacy Case of the Modern State*

I hope to see presentations on many of these topics at this year’s APSA.
