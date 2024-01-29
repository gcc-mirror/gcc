/* Test non-canonical BID significands: _Decimal128, case where
   combination field starts 11.  Bug 91226.  */
/* { dg-do run { target { lp64 && dfprt } } } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu23 -O2" } */

extern void abort (void);
extern void exit (int);

union u
{
  _Decimal128 d128;
  unsigned __int128 u128;
};

#define U128(hi, lo) (((unsigned __int128) lo) \
		      | (((unsigned __int128) hi) << 64))

int
main (void)
{
  unsigned __int128 i = U128 (0x6e79000000000000ULL, 0x1ULL);
  union u x;
  _Decimal128 d128;
  x.u128 = i;
  d128 = x.d128;
  volatile double d = d128;
  if (d != 0)
    abort ();
  /* The above number should have quantum exponent 1234.  */
  _Decimal128 t1233 = 0.e1233DL, t1234 = 0.e1234DL, t1235 = 0.e1235DL;
  _Decimal128 dx;
  dx = d128 + t1233;
  if (__builtin_memcmp (&dx, &t1233, 16) != 0)
    abort ();
  dx = d128 + t1234;
  if (__builtin_memcmp (&dx, &t1234, 16) != 0)
    abort ();
  dx = d128 + t1235;
  if (__builtin_memcmp (&dx, &t1234, 16) != 0)
    abort ();
  exit (0);
}
