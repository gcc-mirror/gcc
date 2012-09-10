/* { dg-options "(-mips16) -mgp64 -EB" } */

typedef unsigned uint128_t __attribute__((mode(TI)));

extern uint128_t g[16];
extern unsigned char gstuff[0x10000];

NOMIPS16 uint128_t
foo (uint128_t i1, uint128_t i2, uint128_t i3, uint128_t i4,
     uint128_t *x, unsigned char *lstuff)
{
  g[0] = i1;
  g[1] = i2;
  g[2] = i3;
  g[3] = i4;
  x[0] = x[4];
  x[1] = 0;
  x[2] = ((uint128_t) 0x123456789abcdefULL << 64) | 0xaabbccddeeff1122ULL;
  x[3] = g[4];
  x[4] = *(uint128_t *) (lstuff + 0x7fff);
  return *(uint128_t *) (gstuff + 0x7fff);
}

MIPS16 uint128_t
bar (uint128_t i1, uint128_t i2, uint128_t i3, uint128_t i4,
     uint128_t *x, unsigned char *lstuff)
{
  g[0] = i1;
  g[1] = i2;
  g[2] = i3;
  g[3] = i4;
  x[0] = x[4];
  x[1] = 0;
  x[2] = ((uint128_t) 0x123456789abcdefULL << 64) | 0xaabbccddeeff1122ULL;
  x[3] = g[4];
  x[4] = *(uint128_t *) (lstuff + 0x7fff);
  return *(uint128_t *) (gstuff + 0x7fff);
}
