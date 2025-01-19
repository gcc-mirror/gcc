/* PR middle-end/117847 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 512
typedef unsigned _BitInt(512) B;

__attribute__((noipa)) B
foo (B a, int r)
{
  B b = __builtin_stdc_rotate_left (a, r);
  return b;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 512
  B a = 0x4ad4fdecc8717d7c6f8b1afb82fdb742477ef2ab34057d1dcc79ba30c38a352dea3253c8c25126d98da02213ad54b90d2998f947941ea4b45e71c61dc1fe3192uwb;
  B x = foo (a, 0);
  if (x != a)
    __builtin_abort ();
#endif
}
