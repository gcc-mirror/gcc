/* PR target/107627 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "\torl\t" } } */

static inline unsigned long long
qux (unsigned int x, unsigned int y)
{
  return ((unsigned long long) x << 32) | y;
}

static inline unsigned int
corge (unsigned int x, unsigned int y, unsigned z)
{
  return qux (x, y) >> (z % 32);
}

void
garply (unsigned int *x, const unsigned int *y, unsigned z)
{
  x[0] = corge (y[0], y[1], z);
}
