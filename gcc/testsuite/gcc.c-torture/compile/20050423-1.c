/* PR tree-optimization/20742 */

int
foo (int x, int y)
{
  register int a = y + 57;
  register int b = y + 31;

  while (x-- > 0)
    {
 #define TEN(x) x x x x x x x x x x
      TEN (TEN (a += b; b -= a;))
      TEN (TEN (a += b; b -= a;))
    }
  return a + b;
}
