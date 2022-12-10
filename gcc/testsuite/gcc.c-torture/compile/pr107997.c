/* PR tree-optimization/107997 */

int a, b;
void bar (int);
int baz (void);

void *
foo (int x, void *y)
{
  asm goto ("" : : "r" (x || !a) : : l);
l:
  if (y)
    return 0;
  bar (b ? b : x);
  while (x--)
    {
      if (!baz ())
	baz ();
      asm goto ("" : : : : l2);
    l2:;
    }
  return y;
}
