/* PR tree-optimization/113370 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 255
_BitInt(255)
foo (int s)
{
  return -(_BitInt(255)) 3 >> s;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 255
  if (foo (51) != -1)
    __builtin_abort ();
#endif
  return 0;
}
