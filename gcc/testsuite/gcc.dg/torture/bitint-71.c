/* PR middle-end/115352 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 385
int
foo (_BitInt (385) b)
{
  return __builtin_sub_overflow_p (0, b, (_BitInt (65)) 0);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 385
  if (!foo (-(_BitInt (385)) 0x00000000000000000c377e8a3fd1881fff035bb487a51c9ed1f7350befa7ec4450000000000000000a3cf8d1ebb723981wb))
    __builtin_abort ();
  if (!foo (-0x1ffffffffffffffffc377e8a3fd1881fff035bb487a51c9ed1f7350befa7ec445ffffffffffffffffa3cf8d1ebb723981uwb))
    __builtin_abort ();
  if (!foo (-(_BitInt (385)) 0x00000000000000000ffffffffffffffffffffffffffffffff00000000000000000000000000000000a3cf8d1ebb723981wb))
    __builtin_abort ();
  if (!foo (-0x1ffffffffffffffff00000000000000000000000000000000ffffffffffffffffffffffffffffffffa3cf8d1ebb723981uwb))
    __builtin_abort ();
#endif
}
