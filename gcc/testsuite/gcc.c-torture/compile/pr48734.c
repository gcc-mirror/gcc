/* PR tree-optimization/48734 */

unsigned int
foo (int x, unsigned int y, unsigned int z)
{
  z &= (x == -__INT_MAX__ - 1 ? x : -x) > y;
  z &= (x == -__INT_MAX__ - 1 ? x : -x) > y;
  z &= (x == -__INT_MAX__ - 1 ? x : -x) > y;
  z &= (x == -__INT_MAX__ - 1 ? x : -x) > y;
  return z;
}
