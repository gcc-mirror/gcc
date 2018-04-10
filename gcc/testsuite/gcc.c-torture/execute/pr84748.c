/* { dg-require-effective-target int128 } */

typedef unsigned __int128 u128;

int a, c, d;
u128 b;

unsigned long long g0, g1;

void
store (unsigned long long a0, unsigned long long a1)
{
  g0 = a0;
  g1 = a1;
}

void
foo (void)
{
  b += a;
  c = d != 84347;
  b /= c;
  u128 x = b;
  store (x >> 0, x >> 64);
}

int
main (void)
{
  foo ();
  if (g0 != 0 || g1 != 0)
    __builtin_abort ();
  return 0;
}
