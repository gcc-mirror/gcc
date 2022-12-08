/* PR target/107627 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "\torq\t" } } */

static inline unsigned __int128
foo (unsigned long long x, unsigned long long y)
{
  return ((unsigned __int128) x << 64) | y;
}

static inline unsigned long long
bar (unsigned long long x, unsigned long long y, unsigned z)
{
  return foo (x, y) >> (z % 64);
}

void
baz (unsigned long long *x, const unsigned long long *y, unsigned z)
{
  x[0] = bar (0xdeadbeefcafebabeULL, y[1], z);
}

void
qux (unsigned long long *x, const unsigned long long *y, unsigned z)
{
  x[0] = bar (y[0], 0xdeadbeefcafebabeULL, z);
}
