/* Test non-canonical BID significands: _Decimal64.  Bug 91226.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-std=gnu2x -O2" } */

extern void abort (void);
extern void exit (int);

union u
{
  _Decimal64 d64;
  unsigned long long int u64;
};

int
main (void)
{
  union u x;
  _Decimal64 d64;
  x.u64 = 0x6c7386f26fc10001ULL;
  d64 = x.d64;
  volatile double d = d64;
  if (d == 0)
    exit (0);
  else
    abort ();
}
