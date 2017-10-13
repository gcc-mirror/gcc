/* PR target/82274 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int
main ()
{
#ifdef __SIZEOF_INT128__
  __int128 m = -(((__int128) 1) << (__CHAR_BIT__ * __SIZEOF_INT128__ / 2));
  volatile __int128 mv = m;
  __int128 r;
#else
  long long m = -(1LL << (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2));
  volatile long long mv = m;
  long long r;
#endif
  if (!__builtin_mul_overflow (mv, mv, &r))
    __builtin_abort ();
  if (!__builtin_mul_overflow (m, m, &r))
    __builtin_abort ();
  return 0;
}
