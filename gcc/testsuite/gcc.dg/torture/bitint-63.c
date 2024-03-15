/* PR libgcc/114327 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256)
foo (_BitInt(256) b, _BitInt(256) c)
{
  return b % c;
}

_BitInt(256)
bar (_BitInt(256) b, _BitInt(256) c)
{
  return b / c;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (foo (-0x9e9b9fe60wb, 1wb))
    __builtin_abort ();
  if (bar (1wb, -0x9e9b9fe60wb))
    __builtin_abort ();
#endif
}
