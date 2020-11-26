/* PR tree-optimization/97979 */

int
foo (int x)
{
  return (x & 0x123) << -3;
}
