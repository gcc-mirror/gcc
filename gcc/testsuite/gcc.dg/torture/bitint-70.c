/* PR libgcc/114762 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 255
__attribute__((__noipa__)) signed _BitInt(255)
foo (signed _BitInt(255) a, signed _BitInt(65) b)
{
  return a / b;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 255
  if (foo (1, -0xffffffffffffffffwb - 1wb))
    __builtin_abort ();
#endif
}
