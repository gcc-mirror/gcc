/* PR tree-optimization/105218 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int a, c;
void bar (void);

void
foo (void)
{
  int b = 131;
  if (a)
    b = c == 2 ? 1 : c;
  while (b)
    bar ();
}
