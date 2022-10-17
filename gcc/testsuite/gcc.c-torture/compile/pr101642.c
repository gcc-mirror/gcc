/* PR middle-end/101642 */

int x;

unsigned short
foo (void)
{
  return __builtin_bswap16 (x) ? : 0;
}

int
bar (int x, int y)
{
  unsigned short a = __builtin_bswap16 ((unsigned short) x);
  unsigned short b = __builtin_bswap16 ((unsigned short) y);
  return a == b;
}
