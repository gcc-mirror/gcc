/* PR tree-optimization/53226 */

void
foo (unsigned long *x, char y, char z)
{
  int i;
  for (i = y; i < z; ++i)
    {
      unsigned long a = ((unsigned char) i) & 63UL;
      unsigned long b = 1ULL << a;
      *x |= b;
    }
}
