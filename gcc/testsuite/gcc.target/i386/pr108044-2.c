/* PR target/108044 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

static inline unsigned long long
foo (unsigned int x, unsigned int y)
{
  return ((unsigned long long) x << 32) | y;
}

void
bar (unsigned long long *p, unsigned int x)
{
  p[0] = foo (x, 0xcafebabeU);
}

void
baz (unsigned long long *p, unsigned int x)
{
  p[0] = foo (0xcafebabeU, x);
}
