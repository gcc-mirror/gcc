/* PR tree-optimization/114040 */
/* { dg-do run { target { bitint && int128 } } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

unsigned a;
signed char b;
short c;
long d;
__int128 e;
int f;

#if __BITINT_MAXWIDTH__ >= 511
__attribute__((noinline)) void
foo (_BitInt(3) x, unsigned _BitInt(511) y, unsigned *z)
{
  int g = __builtin_sub_overflow_p (y ^ x, 0, (unsigned _BitInt(255)) 0);
  unsigned h = y + e, i = h + d;
  unsigned _BitInt(2) j = i + g;
  unsigned k = j + c;
  unsigned l = k + a + f + b;
  *z = l;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 511
  unsigned x;
  foo (0, 0x81e4a5fa7c408f370000000000000000uwb, &x);
  if (x)
    __builtin_abort ();
#endif
}
