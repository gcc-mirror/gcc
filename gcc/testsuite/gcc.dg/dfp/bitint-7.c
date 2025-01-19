/* PR c/102989 */
/* Test non-canonical BID significands.  */
/* { dg-require-effective-target bitint } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu23 -O2" } */

union U32
{
  _Decimal32 d;
  unsigned int u;
};

union U64
{
  _Decimal64 d;
  unsigned long long int u;
};

union U128
{
  _Decimal128 d;
  unsigned long long int u[2];
};

int
main ()
{
  volatile union U32 u32;
  u32.d = 0.9999999e+27DF;
  u32.u++;
  volatile union U64 u64;
  u64.d = 0.9999999999999999e+90DD;
  u64.u++;
  volatile union U128 u128;
  u128.d = 0.9999999999999999999999999999999999e+39DL;
  if (u128.u[0] == 0x378d8e63ffffffffULL)
    u128.u[0]++;
  else if (u128.u[1] == 0x378d8e63ffffffffULL)
    u128.u[1]++;
  else
    u128.d = 0.DL;
#if __BITINT_MAXWIDTH__ >= 192
  if ((_BitInt(192)) u32.d != 0wb
      || (unsigned _BitInt(192)) u32.d != 0uwb
      || (_BitInt(192)) u64.d != 0wb
      || (unsigned _BitInt(192)) u64.d != 0uwb
      || (_BitInt(192)) u128.d != 0wb
      || (unsigned _BitInt(192)) u128.d != 0uwb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 575
  if ((_BitInt(575)) u32.d != 0wb
      || (unsigned _BitInt(575)) u32.d != 0uwb
      || (_BitInt(575)) u64.d != 0wb
      || (unsigned _BitInt(575)) u64.d != 0uwb
      || (_BitInt(575)) u128.d != 0wb
      || (unsigned _BitInt(575)) u128.d != 0uwb)
    __builtin_abort ();
#endif
  u32.u = 0xe59fffffU;
  u64.u = 0xe3ffffffffffffffULL;
  if (u128.u[0] == 0x378d8e6400000000ULL)
    {
      u128.u[0] = -1ULL;
      u128.u[1] = 0xe1be7fffffffffffULL;
    }
  else if (u128.u[1] == 0x378d8e6400000000ULL)
    {
      u128.u[1] = -1ULL;
      u128.u[0] = 0xe1be7fffffffffffULL;
    }
#if __BITINT_MAXWIDTH__ >= 192
  if ((_BitInt(192)) u32.d != 0wb
      || (unsigned _BitInt(192)) u32.d != 0uwb
      || (_BitInt(192)) u64.d != 0wb
      || (unsigned _BitInt(192)) u64.d != 0uwb
      || (_BitInt(192)) u128.d != 0wb
      || (unsigned _BitInt(192)) u128.d != 0uwb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 575
  if ((_BitInt(575)) u32.d != 0wb
      || (unsigned _BitInt(575)) u32.d != 0uwb
      || (_BitInt(575)) u64.d != 0wb
      || (unsigned _BitInt(575)) u64.d != 0uwb
      || (_BitInt(575)) u128.d != 0wb
      || (unsigned _BitInt(575)) u128.d != 0uwb)
    __builtin_abort ();
#endif
  if (u128.u[0] == -1ULL)
    {
      u128.u[0] = 0;
      u128.u[1] = 0xe629800000000000ULL;
    }
  else if (u128.u[1] == -1ULL)
    {
      u128.u[1] = 0;
      u128.u[0] = 0xe629800000000000ULL;
    }
#if __BITINT_MAXWIDTH__ >= 192
  if ((_BitInt(192)) u128.d != 0wb
      || (unsigned _BitInt(192)) u128.d != 0uwb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 575
  if ((_BitInt(575)) u128.d != 0wb
      || (unsigned _BitInt(575)) u128.d != 0uwb)
    __builtin_abort ();
#endif
}
