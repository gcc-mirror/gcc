/* PR target/93333 */

unsigned
foo (int b, int c, int d, unsigned long e, int x, int y, int g, int h,
     unsigned i)
{
  e >>= b;
  i >>= e & 31;
  return i & 1;
}
