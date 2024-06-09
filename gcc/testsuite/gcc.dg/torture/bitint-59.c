/* PR tree-optimization/114038 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 129
int
foo (unsigned _BitInt(63) x, unsigned _BitInt(129) y)
{
  return __builtin_mul_overflow_p (y, x, 0);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  if (!foo (90, 0x80000000000000000000000000000000uwb))
    __builtin_abort ();
#endif
}
