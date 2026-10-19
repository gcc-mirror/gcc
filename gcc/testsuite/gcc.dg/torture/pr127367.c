/* PR rtl-optimization/127367 */
/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 96
_BitInt(5) g;
_BitInt(65) prod;

__attribute__((noipa)) void
foo (void)
{
  _BitInt(6) t = ~g;
  _BitInt(96) m = t % (unsigned _BitInt(96))362;
  _BitInt(5) r = __builtin_mul_overflow(t, m, &prod) ?: prod;
  if ((long long)m != 297 || (int)r != -9 || (long long)prod != -297)
    __builtin_abort ();
}
#endif

int
main (void)
{
#if __BITINT_MAXWIDTH__ >= 96
  foo ();
#endif
  return 0;
}
