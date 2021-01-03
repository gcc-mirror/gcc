/* PR debug/97989 */
/* { dg-do preprocess } */
/* { dg-options "-g3 -g2 -P" } */

#define foo bar
int i;

/* { dg-final { scan-file-not pr97989-1.i "(^|\\n)#define foo bar($|\\n)" } } */
