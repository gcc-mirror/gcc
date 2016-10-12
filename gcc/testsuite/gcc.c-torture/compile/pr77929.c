/* PR tree-optimization/77929 */

void bar (void);

void
foo (int x, unsigned short int y)
{
  int a = 0;
  int b = (y != 0) ? (x < y) : (a < 0);

  if (x >= 0 & b)
    bar ();
}
