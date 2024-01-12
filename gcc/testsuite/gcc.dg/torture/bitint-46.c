/* PR tree-optimization/113334 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 384
__attribute__((noipa)) _BitInt(384)
foo (int s)
{
  _BitInt(384) z = (-(unsigned _BitInt(384)) 4) >> s;
  return z;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 384
  if (foo (59) != 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb)
    __builtin_abort ();
  if (foo (0) != -4wb)
    __builtin_abort ();
  if (foo (1) != 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffewb)
    __builtin_abort ();
  if (foo (11) != 0x001fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb)
    __builtin_abort ();
  if (foo (123) != 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb)
    __builtin_abort ();
#endif
  return 0;
}
