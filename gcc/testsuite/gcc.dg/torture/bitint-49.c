/* PR tree-optimization/113372 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O1" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

_BitInt(8) a, b, c;

#if __BITINT_MAXWIDTH__ >= 6384
_BitInt(8)
foo (_BitInt(6384) y)
{
  _BitInt(4745) x = -(b % y) * b;
  int i = __builtin_sub_overflow_p (-y, 0, 0);
  c |= __builtin_add_overflow_p (i, 0, a);
  return x;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 6384
  if (foo (4) != 0 || c != 0)
    __builtin_abort ();
#endif
  return 0;
}
