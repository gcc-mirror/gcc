/* PR target/108044 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fcf-protection=branch" } */

static inline unsigned long long
foo (unsigned int x, unsigned int y)
{
  return ((unsigned long long) x << 32) | y;
}

unsigned long long
bar (unsigned int x)
{
  return foo (x, 0xfa1e0ff3U);
}

unsigned long long
baz (unsigned int x)
{
  return foo (0xfa1e0ff3U, x);
}
