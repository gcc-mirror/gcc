/* PR target/108044 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

static inline unsigned __int128
foo (unsigned long long x, unsigned long long y)
{
  return ((unsigned __int128) x << 64) | y;
}

void
bar (unsigned __int128 *p, unsigned long long x)
{
  p[0] = foo (x, 0xdeadbeefcafebabeULL);
}

void
baz (unsigned __int128 *p, unsigned long long x)
{
  p[0] = foo (0xdeadbeefcafebabeULL, x);
}

void
qux (unsigned __int128 *p, unsigned long long x)
{
  p[0] = foo (x, 0xffffffffcafebabeULL);
}

void
corge (unsigned __int128 *p, unsigned long long x)
{
  p[0] = foo (0xffffffffcafebabeULL, x);
}
