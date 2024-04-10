/* PR tree-optimization/113361 */
/* { dg-do run { target { bitint && int128 } } } */
/* { dg-options "-std=gnu23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 129
int
foo (_BitInt(65) x)
{
  return __builtin_mul_overflow_p ((__int128) 0xffffffff << 64, x, (_BitInt(129)) 0);
}

int
bar (_BitInt(63) x)
{
  return __builtin_mul_overflow_p ((__int128) 0xffffffff << 64, x, (_BitInt(129)) 0);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  if (!foo (5167856845))
    __builtin_abort ();
  if (!bar (5167856845))
    __builtin_abort ();
#endif
  return 0;
}
