/* PR middle-end/127517 */
/* { dg-do run { target bitint } } */

__attribute__((noipa)) void
foo (int x)
{
#if __BITINT_MAXWIDTH__ >= 128
  _BitInt(128) t = 0x7fffffffffffffffffffffffffffffffwb;
  if (!__builtin_mul_overflow_p (t, x, (_BitInt (127)) 0))
    __builtin_abort ();
#endif
}

int
main ()
{
  foo (8);
}
