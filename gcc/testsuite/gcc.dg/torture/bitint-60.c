/* PR tree-optimization/114040 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 8671
__attribute__((noipa)) unsigned
foo (unsigned _BitInt(8671) x, unsigned y, unsigned _BitInt(512) z)
{
  unsigned _BitInt (8671) r
    = x * __builtin_sub_overflow_p (y * z, 0, (unsigned _BitInt(255)) 0);
  return r;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 8671
  if (foo (1, 1, 0xfffa46471e7c2dd60000000000000000wb))
    __builtin_abort ();
#endif
}
