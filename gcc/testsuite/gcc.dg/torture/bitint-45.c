/* PR middle-end/112750 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256) a = __INT_MAX__ + (_BitInt(256)) 1;
_BitInt(256) b = __INT_MAX__;
#endif
#if __BITINT_MAXWIDTH__ >= 512
_BitInt(512) c = 0x7fffffffffffffffffffffffffffffffffffffffwb + (_BitInt(512)) 1;
_BitInt(512) d = 0x7fffffffffffffffffffffffffffffffffffffffwb;
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (!__builtin_sub_overflow_p (a, 0, 0))
    __builtin_abort ();
  if (!__builtin_sub_overflow_p (b, -1, 0))
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 512
  if (!__builtin_sub_overflow_p (c, 0, (_BitInt(160)) 0))
    __builtin_abort ();
  if (!__builtin_sub_overflow_p (d, -1, 0))
    __builtin_abort ();
#endif
  return 0;
}
