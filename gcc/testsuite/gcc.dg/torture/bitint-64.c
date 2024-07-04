/* PR middle-end/114332 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -fwrapv" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

enum E { E22 = 22 } e = E22;

_BitInt (5)
foo (void)
{
  _Atomic _BitInt (5) b = 0;
  b += e;
  return b;
}

int
main ()
{
  if (foo () != -10)
    __builtin_abort ();
}
