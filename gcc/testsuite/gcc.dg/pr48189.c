/* PR tree-optimization/48189 */
/* { dg-do compile } */
/* { dg-options "-O --param max-predicted-iterations=0" } */

struct S { int s[8]; };
  
void
foo (int *x, struct S *y)
{
  int i;
  for (i = 0; y[i].s[i]; i++)
    *x++ = y[i].s[i];
}
