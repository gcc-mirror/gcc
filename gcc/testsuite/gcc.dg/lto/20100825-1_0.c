/* { dg-lto-do link } */
/* { dg-lto-options { { -O3 -flto } } } */

typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef struct { UINT64 w[2]; } UINT128;
void _bid_to_dpd128 (UINT128 *, UINT128 *);
static const int short_recip_scale[] = {
  1,
  65 - 64,
  69 - 64,
  71 - 64,
  75 - 64,
  78 - 64,
  81 - 64,
  85 - 64,
  88 - 64,
  91 - 64,
  95 - 64,
  98 - 64,
  101 - 64,
  105 - 64,
  108 - 64,
  111 - 64,
  115 - 64,
  118 - 64
};

void _bid_to_dpd128 (UINT128 *, UINT128 *);

void
_bid_to_dpd128 (UINT128 *pres, UINT128 *px) {
  UINT128 res;
  unsigned int comb;
  UINT128 bcoeff;
  UINT128 BH;
  UINT64 BL, d109;
  unsigned int amount;
  UINT128 x = *px;

  comb = (x.w[1] ) >> 46;
  if ((comb & 0x1e000) == 0x1e000) {
    res = x;
  } else {
    bcoeff.w[1] = (x.w[1] & 0x0001ffffffffffffull);
    bcoeff.w[0] = x.w[0];
    amount = 9;
    BH.w[0] = (BH.w[0] >> amount) | (BH.w[1] << (64 - amount));
    BL = bcoeff.w[0] - BH.w[0] * 1000000000000000000ull;
    d109 = 0x3333333333333334ull;
    { UINT64 CXH, CXL, CYH,CYL,PL,PH,PM,PM2; CXH = (BH.w[0]) >> 32; CXL = (UINT32)(BH.w[0]); CYH = (d109) >> 32; CYL = (UINT32)(d109); PM = CXH*CYL; PH = CXH*CYH; PL = CXL*CYL; PM2 = CXL*CYH; PH += (PM>>32); PM = (UINT64)((UINT32)PM)+PM2+(PL>>32); };
    { UINT64 CXH, CXL, CYH,CYL,PL,PH,PM,PM2; CXH = (BL) >> 32; CXL = (UINT32)(BL); CYH = (d109) >> 32; CYL = (UINT32)(d109); PM = CXH*CYL; PH = CXH*CYH; PL = CXL*CYL; PM2 = CXL*CYH; PH += (PM>>32); PM = (UINT64)((UINT32)PM)+PM2+(PL>>32);  };
  }
  *pres = res;
}

int main() { return 0; }
