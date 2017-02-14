/* PR tree-optimization/77901 */

void bar (void);

void
foo (int *x, long *y)
{
  if (*y && *x != 10 && *x != 12 && *y >= 0)
    bar ();
}
