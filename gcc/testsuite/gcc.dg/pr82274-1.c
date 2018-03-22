/* PR target/82274 */
/* { dg-do run } */
/* { dg-shouldfail "trapv" } */
/* { dg-options "-ftrapv" } */

int
main ()
{
#ifdef __SIZEOF_INT128__
  volatile __int128 m = -(((__int128) 1) << (__CHAR_BIT__ * __SIZEOF_INT128__ / 2));
#else
  volatile long long m = -(1LL << (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2));
#endif
  m = m * m;
  return 0;
}
