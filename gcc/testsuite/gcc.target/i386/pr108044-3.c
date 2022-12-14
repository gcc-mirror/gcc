/* PR target/108044 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fcf-protection=branch" } */

static inline unsigned __int128
foo (unsigned long long x, unsigned long long y)
{
  return ((unsigned __int128) x << 64) | y;
}

unsigned __int128
bar (unsigned long long x)
{
  return foo (x, 0xfa1e0ff3ULL);
}

unsigned __int128
baz (unsigned long long x)
{
  return foo (0xfa1e0ff3ULL, x);
}

unsigned __int128
qux (unsigned long long x)
{
  return foo (x, 0xffbafa1e0ff3abdeULL);
}

unsigned __int128
corge (unsigned long long x)
{
  return foo (0xffbafa1e0ff3abdeULL, x);
}
