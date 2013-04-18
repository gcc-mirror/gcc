/* PR tree-optimization/56984 */

int
foo (int x)
{
  if ((x >> 31) < -1)
    x++;
  return x;
}
