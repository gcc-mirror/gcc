/* { dg-do compile } */

typedef __UINT64_TYPE__ UINT64;
typedef union {
    struct {
	unsigned short lo4;
	unsigned short lo3;
	unsigned short lo2;
	unsigned short lo1;
    } i;
    long double f;
} BID_BINARY80LDOUBLE;
UINT64 __binary80_to_bid32 (long double x)
{
  BID_BINARY80LDOUBLE x_in;
  x_in.f = x;
  return (x_in.i.lo4
	  + ((UINT64)x_in.i.lo3 << 16)
	  + ((UINT64)x_in.i.lo2 << 32)
	  + ((UINT64)x_in.i.lo1 << 48));
}
