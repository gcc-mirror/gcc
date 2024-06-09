/* PR tree-optimization/111913 */

int f(unsigned int x, unsigned int y)
{
  return __builtin_popcount (x&y) + __builtin_popcount (y|x--);
}

int f2(unsigned int x, unsigned int y)
{
  int t = __builtin_popcount (x&y);
  int t1 = __builtin_popcount (x|y);
  return t + t1;
}
