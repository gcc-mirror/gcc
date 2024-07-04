/* Test non-canonical BID significands: _Decimal32.  Bug 91226.  */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu23 -O2" } */

extern void abort (void);
extern void exit (int);

union u
{
  _Decimal32 d32;
  unsigned int u32;
};

int
main (void)
{
  union u x;
  _Decimal32 d32;
  x.u32 = 0x6cb89681U;
  d32 = x.d32;
  volatile double d = d32;
  if (d == 0)
    exit (0);
  else
    abort ();
}
