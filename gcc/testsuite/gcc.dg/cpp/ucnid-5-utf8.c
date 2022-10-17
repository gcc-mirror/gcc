/* { dg-do preprocess } */
/* { dg-options "-std=c99 -pedantic" } */

ª
« /* not a preprocessing error because we lex it into its own token */
¶ /* not a preprocessing error because we lex it into its own token */
º
À
Ö
΄ /* not a preprocessing error because we lex it into its own token */

٩ /* { dg-error "not valid at the start of an identifier" } */
A٩
0º
0٩
๙ /* { dg-error "not valid at the start of an identifier" } */
A๙
