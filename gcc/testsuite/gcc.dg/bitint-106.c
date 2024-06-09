/* PR tree-optimization/115337 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 129
#define N 128
#else
#define N 63
#endif

_BitInt (N) g;
int c;

void
foo (unsigned _BitInt (N + 1) z, _BitInt (N) *ret)
{
  c = __builtin_stdc_first_leading_one (z << N);
  _BitInt (N) y = *(_BitInt (N) *) __builtin_memset (&g, c, 5);
  *ret = y;
}

int
main ()
{
  _BitInt (N) x;
  foo (0, &x);
  if (c || g || x)
    __builtin_abort ();
}
