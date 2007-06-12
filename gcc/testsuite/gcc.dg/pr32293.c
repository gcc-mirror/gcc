/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int _IDEC_glbround;
unsigned int _IDEC_glbflags;
typedef unsigned UINT32;
typedef signed SINT32;
typedef unsigned long long UINT64;
typedef signed long long SINT64;
typedef
__attribute__ ((aligned(16)))
     struct {
       UINT64 w[2];
     } UINT128;

static __inline UINT64
unpack_BID128 (UINT64 * psign_x, int *pexponent_x,
        UINT128 * pcoefficient_x, UINT128 * px) {
  UINT128 coeff;
  UINT64 ex;
  *psign_x = (px->w[1]) & 0x8000000000000000ull;
  ex = (px->w[1]) >> 49;
  *pexponent_x = ((int) ex) & 0x3fff;
  return coeff.w[0] | coeff.w[1];
}

static __inline UINT32
get_BID32 (UINT32 sgn, int expon, UINT64 coeff, int rmode,
    unsigned *fpsc) {
  UINT32 r;

  if (((unsigned) expon) > 191) {
      r = sgn | 0x78000000ul;
      switch (rmode) {
      case 0x00002:
        if (sgn)
          r = sgn | 0x77f8967f;
      }
      return r;
  }
  r = expon;
  return r;
}

UINT32
bid128_to_bid32 (UINT128 x)
{
  UINT128 *px;
  UINT128 CX;
  UINT64 sign_x;
  UINT32 res;
  int exponent_x = 0;
  px = &x;
  if (!unpack_BID128 (&sign_x, &exponent_x, &CX, px)) {
      return(res);
  }
  res = get_BID32 ((UINT32) (sign_x >> 32),
        exponent_x, CX.w[0], _IDEC_glbround, &_IDEC_glbflags);
  return(res);;
}

