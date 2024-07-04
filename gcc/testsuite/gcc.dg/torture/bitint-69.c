/* PR libgcc/114755 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 255
_BitInt(65)
foo (void)
{
  _BitInt(255) a = 0x040404040404040404040404wb;
  _BitInt(65) b = -0xffffffffffffffffwb;
  _BitInt(65) r = a % b;
  return r;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 255
  _BitInt(65) x = foo ();
  if (x != 0x0404040408080808wb)
    __builtin_abort ();
#endif
}
