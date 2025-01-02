/* PR tree-optimization/116501 */
/* { dg-do run { target bitint575 } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

_BitInt (4) a;

int
foo (_BitInt(513) b)
{
  return __builtin_sub_overflow_p (a, b, (_BitInt (511)) 0);
}

int
main ()
{
  if (!foo (0xffffffffffffffff0000000000000000ffffffffffffffff0000000000000000ffffffffffffffff0000000000000000ffffffffffffffff0000000000000000wb))
    __builtin_abort ();
}
