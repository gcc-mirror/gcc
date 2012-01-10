/* PR tree-optimization/51247 */

struct S { int s : 1; };
int a;

void
foo (int x, int y)
{
  struct S s;
  s.s = !!y;
  while (1)
    {
      unsigned l = 94967295;
      a = x || (s.s &= l);
    }
}
