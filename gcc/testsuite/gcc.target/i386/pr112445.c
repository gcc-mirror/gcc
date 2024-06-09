/* { dg-do compile { target int128 } } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O -march=cascadelake -fwrapv" } */

typedef _Decimal64 d64;
int foo0_f128_0, foo0_ret, foo0_s64_0;
_Complex float foo0_cf128_0;

void
foo (char u8_0, char s8_0, _Complex unsigned cu8_0, int cs32_0,
      _Complex _Float16 cf16_0, _Complex int cf32_0, int d32_0,
      _Decimal64 d64_0)
{
  cu8_0 *= (__int128) foo0_s64_0;
  int cf32_1 = __builtin_ccosf (cu8_0);
  __int128 u128_r =
    foo0_f128_0 + (__int128) foo0_cf128_0 + (__int128) __imag__ foo0_cf128_0;
  int u64_r = u128_r + foo0_s64_0 + d64_0;
  int u32_r = u64_r + cs32_0 + cf32_0 + __imag__ cf32_0 + cf32_1 + d32_0;
  short u16_r = u32_r + cf16_0 + __imag__ cf16_0;
  char u8_r = u16_r + u8_0 + s8_0 + cu8_0 + __imag__ cu8_0;
  foo0_ret = u8_r;
}
