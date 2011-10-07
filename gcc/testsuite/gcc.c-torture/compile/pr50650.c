/* PR tree-optimization/50650 */

unsigned int
foo (unsigned int x, unsigned int y)
{
  int i;
  for (i = 8; i--; x <<= 1)
    y ^= (x ^ y) & 0x80 ? 79U : 0U;
  return y;
}
