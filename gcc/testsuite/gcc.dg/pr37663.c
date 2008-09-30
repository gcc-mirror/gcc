/* PR tree-optimization/37663 */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv" } */

extern void bar (void);

void
foo (int x)
{
  x = 1 >= x;
  int y = -1885403717;
  x = x + (x != y * y);
  if (x)
    bar ();
}
