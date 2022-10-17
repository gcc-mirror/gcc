/* Test non-canonical BID significands: _Decimal128.  Bug 91226.  */
/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu2x -O2" } */

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
  unsigned __int128 i = U128 (0x3041ed09bead87c0ULL, 0x378d8e6400000001ULL);
  union u x;
  _Decimal128 d128;
  x.u128 = i;
  d128 = x.d128;
  volatile double d = d128;
  if (d == 0)
    exit (0);
  else
    abort ();
}
