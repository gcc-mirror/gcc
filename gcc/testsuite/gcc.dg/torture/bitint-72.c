/* PR tree-optimization/116224 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 65
#define N 65
#else
#define N 63
#endif

signed char g;

int
foo (signed char c, int i, _BitInt(N) b)
{
  __builtin_memmove (&g, &b, 1);
  return b / i / c;
}

int
main ()
{
  int x = foo (-15, -15, 900);
  if (x != 4)
    __builtin_abort ();
}
