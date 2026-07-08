/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop-details -fdump-tree-widening_mul-details" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef struct { uint32_t v[2]; } v2i32;
typedef struct { uint64_t v[2]; } v2i64;

uint64_t mulh_ladder (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = y_lo * x_lo;
  uint64_t t1 = y_lo * x_hi;
  uint64_t t2 = y_hi * x_lo;
  uint64_t t3 = y_hi * x_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t u0 = t0_hi + t1;
  uint64_t u0_lo = u0 & 0xFFFFFFFF;
  uint64_t u0_hi = u0 >> 32;
  uint64_t u1 = u0_lo + t2;
  uint64_t u1_hi = u1 >> 32;
  uint64_t u2 = u0_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

uint64_t mulh_ladder_comm (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = x_lo * y_lo;
  uint64_t t1 = x_lo * y_hi;
  uint64_t t2 = x_hi * y_lo;
  uint64_t t3 = x_hi * y_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t u0 = t1 + t0_hi;
  uint64_t u0_lo = u0 & 0xFFFFFFFF;
  uint64_t u0_hi = u0 >> 32;
  uint64_t u1 = t2 + u0_lo;
  uint64_t u1_hi = u1 >> 32;
  uint64_t u2 = u1_hi + u0_hi;
  uint64_t hw64 = t3 + u2;

  return hw64;
}

uint32_t mulh_ladder_32 (uint32_t x, uint32_t y)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_hi = y >> 16;
  uint32_t t0 = y_lo * x_lo;
  uint32_t t1 = y_lo * x_hi;
  uint32_t t2 = y_hi * x_lo;
  uint32_t t3 = y_hi * x_hi;
  uint32_t t0_hi = t0 >> 16;
  uint32_t u0 = t0_hi + t1;
  uint32_t u0_lo = u0 & 0xFFFF;
  uint32_t u0_hi = u0 >> 16;
  uint32_t u1 = u0_lo + t2;
  uint32_t u1_hi = u1 >> 16;
  uint32_t u2 = u0_hi + t3;
  uint32_t hw64 = u2 + u1_hi;

  return hw64;
}

/* The 128-bit variant lowers to longhand in pass_optimize_widening_mul;
   no target provides a 256-bit multiply.  */
#ifdef __SIZEOF_INT128__
__uint128_t umulh_variant_i128 (__uint128_t x, __uint128_t y)
{
  __uint128_t x_lo = x & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t y_lo = y & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t x_hi = x >> 64;
  __uint128_t y_hi = y >> 64;
  __uint128_t t0 = y_lo * x_lo;
  __uint128_t t1 = y_lo * x_hi;
  __uint128_t t2 = y_hi * x_lo;
  __uint128_t t3 = y_hi * x_hi;
  __uint128_t t0_hi = t0 >> 64;
  __uint128_t u0 = t0_hi + t1;
  __uint128_t u0_lo = u0 & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t u0_hi = u0 >> 64;
  __uint128_t u1 = u0_lo + t2;
  __uint128_t u1_hi = u1 >> 64;
  __uint128_t u2 = u0_hi + t3;
  __uint128_t hw64 = u2 + u1_hi;

  return hw64;
}
#endif

v2i64 full_mul_ladder (uint64_t x, uint64_t y)
{
  uint64_t and_x = x & 0xFFFFFFFF;
  uint64_t and_y = y & 0xFFFFFFFF;
  uint64_t mul_i = and_y * and_x;
  uint64_t shr_x = x >> 32;
  uint64_t mul_i27 = and_y * shr_x;
  uint64_t shr_y = y >> 32;
  uint64_t mul_i28 = shr_y * and_x;
  uint64_t mul_i29 = shr_y * shr_x;
  uint64_t shr10 = mul_i >> 32;
  uint64_t and11 = mul_i27 & 0xFFFFFFFF;
  uint64_t add = and11 + mul_i28;
  uint64_t add12 = add + shr10;
  uint64_t shr13 = mul_i27 >> 32;
  uint64_t shr14 = add12 >> 32;
  uint64_t add15 = shr13 + mul_i29;
  uint64_t add16 = add15 + shr14;
  uint64_t shl = add12 << 32;
  uint64_t and17 = mul_i & 0xFFFFFFFF;
  uint64_t or_val = shl | and17;
  v2i64 result;
  result.v[0] = or_val;
  result.v[1] = add16;
  return result;
}

/* This will be optimized during the second forwprop run.
   Disable SLP so the expected fold count is target-independent.  */
__attribute__((optimize("no-tree-slp-vectorize")))
v2i32 mulh_ladder_v2i32 (v2i32 x, v2i32 y)
{
  v2i32 result;
  for(int i=0; i<2; ++i)
    {
      uint32_t x_lo = x.v[i] & 0xFFFF;
      uint32_t y_lo = y.v[i] & 0xFFFF;
      uint32_t x_hi = x.v[i] >> 16;
      uint32_t y_hi = y.v[i] >> 16;
      uint32_t t0 = y_lo * x_lo;
      uint32_t t1 = y_lo * x_hi;
      uint32_t t2 = y_hi * x_lo;
      uint32_t t3 = y_hi * x_hi;
      uint32_t t0_hi = t0 >> 16;
      uint32_t u0 = t0_hi + t1;
      uint32_t u0_lo = u0 & 0xFFFF;
      uint32_t u0_hi = u0 >> 16;
      uint32_t u1 = u0_lo + t2;
      uint32_t u1_hi = u1 >> 16;
      uint32_t u2 = u0_hi + t3;
      result.v[i] = u2 + u1_hi;
    }

  return result;
}

/* Ladder-long variants: hi-part sum uses the long form
   (xh*yh + cross_hi_a + cross_hi_b + mid_hi).  */

uint32_t mulh_ladder_long (uint32_t x, uint32_t y)
{
  uint32_t xl = x & 0xFFFF;
  uint32_t xh = x >> 16;
  uint32_t yl = y & 0xFFFF;
  uint32_t yh = y >> 16;
  uint32_t mulll = xl * yl;
  uint32_t mullh = xl * yh;
  uint32_t mulhl = xh * yl;
  uint32_t mulhh = xh * yh;
  uint32_t shr8 = mulll >> 16;
  uint32_t conv10 = mullh & 0xFFFF;
  uint32_t add = shr8 + conv10;
  uint32_t conv12 = mulhl & 0xFFFF;
  uint32_t add13 = add + conv12;
  uint32_t shr14 = add13 >> 16;
  uint32_t shr15 = mullh >> 16;
  uint32_t add16 = mulhh + shr15;
  uint32_t shr17 = mulhl >> 16;
  uint32_t add18 = add16 + shr17;
  uint32_t add19 = add18 + shr14;

  return add19;
}

void full_mul_ladder_long (uint32_t x, uint32_t y, uint32_t *p)
{
  uint32_t xl = x & 0xFFFF;
  uint32_t xh = x >> 16;
  uint32_t yl = y & 0xFFFF;
  uint32_t yh = y >> 16;
  uint32_t mulll = xl * yl;
  uint32_t mullh = xl * yh;
  uint32_t mulhl = xh * yl;
  uint32_t mulhh = xh * yh;
  uint32_t shr8 = mulll >> 16;
  uint32_t conv10 = mullh & 0xFFFF;
  uint32_t add = shr8 + conv10;
  uint32_t conv12 = mulhl & 0xFFFF;
  uint32_t add13 = add + conv12;
  uint32_t shr14 = add13 >> 16;
  uint32_t shr15 = mullh >> 16;
  uint32_t add16 = mulhh + shr15;
  uint32_t shr17 = mulhl >> 16;
  uint32_t add18 = add16 + shr17;
  uint32_t add19 = add18 + shr14;
  p[1] = add19;
  uint32_t add_13_shl = add13 << 16;
  uint32_t and17 = mulll & 0xFFFF;
  uint32_t or_val = add_13_shl | and17;
  p[0] = or_val;
}

uint32_t mulh_ladder_long_comm (uint32_t x, uint32_t y)
{
  uint32_t xl = x & 0xFFFF;
  uint32_t xh = x >> 16;
  uint32_t yl = y & 0xFFFF;
  uint32_t yh = y >> 16;
  uint32_t mulll = yl * xl;
  uint32_t mullh = yh * xl;
  uint32_t mulhl = yl * xh;
  uint32_t mulhh = yh * xh;
  uint32_t shr8 = mulll >> 16;
  uint32_t conv10 = mullh & 0xFFFF;
  uint32_t add = conv10 + shr8;
  uint32_t conv12 = mulhl & 0xFFFF;
  uint32_t add13 = conv12 + add;
  uint32_t shr14 = add13 >> 16;
  uint32_t shr15 = mullh >> 16;
  uint32_t shr17 = mulhl >> 16;
  uint32_t add16 = shr14 + shr17;
  uint32_t add18 = add16 + shr15;
  uint32_t add19 = mulhh + add18;

  return add19;
}

/* The 128-bit variant lowers to longhand in pass_optimize_widening_mul;
   no target provides a 256-bit multiply.  */
#ifdef __SIZEOF_INT128__
__uint128_t mulh_ladder_long_128 (__uint128_t x, __uint128_t y)
{
  __uint128_t xl = x & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t xh = x >> 64;
  __uint128_t yl = y & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t yh = y >> 64;
  __uint128_t mulll = xl * yl;
  __uint128_t mullh = xl * yh;
  __uint128_t mulhl = xh * yl;
  __uint128_t mulhh = xh * yh;
  __uint128_t shr8 = mulll >> 64;
  __uint128_t conv10 = mullh & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t add = shr8 + conv10;
  __uint128_t conv12 = mulhl & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t add13 = add + conv12;
  __uint128_t shr14 = add13 >> 64;
  __uint128_t shr15 = mullh >> 64;
  __uint128_t add16 = mulhh + shr15;
  __uint128_t shr17 = mulhl >> 64;
  __uint128_t add18 = add16 + shr17;
  __uint128_t add19 = add18 + shr14;

  return add19;
}
#endif

uint32_t mulh_ladder_long_hllh (uint32_t x, uint32_t y)
{
  uint32_t xl = x & 0xFFFF;
  uint32_t xh = x >> 16;
  uint32_t yl = y & 0xFFFF;
  uint32_t yh = y >> 16;
  uint32_t mulll = xl * yl;
  uint32_t mullh = xl * yh;
  uint32_t mulhl = xh * yl;
  uint32_t mulhh = xh * yh;
  uint32_t shr8 = mulll >> 16;
  uint32_t conv10 = mulhl & 0xFFFF;
  uint32_t add = shr8 + conv10;
  uint32_t conv12 = mullh & 0xFFFF;
  uint32_t add13 = add + conv12;
  uint32_t shr14 = add13 >> 16;
  uint32_t shr15 = mulhl >> 16;
  uint32_t add16 = mulhh + shr15;
  uint32_t shr17 = mullh >> 16;
  uint32_t add18 = add16 + shr17;
  uint32_t add19 = add18 + shr14;

  return add19;
}

/* This will be optimized during the second forwprop run.
   Disable SLP so the expected fold count is target-independent.  */
__attribute__((optimize("no-tree-slp-vectorize")))
v2i32 mul_ladder_long_v2i32 (v2i32 x, v2i32 y)
{
  v2i32 result;
  for (int i = 0; i < 2; i++)
    {
      uint32_t xl = x.v[i] & 0xFFFF;
      uint32_t xh = x.v[i] >> 16;
      uint32_t yl = y.v[i] & 0xFFFF;
      uint32_t yh = y.v[i] >> 16;
      uint32_t mulll = xl * yl;
      uint32_t mullh = xl * yh;
      uint32_t mulhl = xh * yl;
      uint32_t mulhh = xh * yh;
      uint32_t shr8 = mulll >> 16;
      uint32_t conv10 = mullh & 0xFFFF;
      uint32_t add = shr8 + conv10;
      uint32_t conv12 = mulhl & 0xFFFF;
      uint32_t add13 = add + conv12;
      uint32_t shr14 = add13 >> 16;
      uint32_t shr15 = mullh >> 16;
      uint32_t add16 = mulhh + shr15;
      uint32_t shr17 = mulhl >> 16;
      uint32_t add18 = add16 + shr17;
      result.v[i] = add18 + shr14;
    }

    return result;
}

/* On targets with __int128 support the 128-bit highpart also folds;
   without it it is elided by #ifdef and the count drops by 2.  */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 10 "forwprop1" { target { oi_mode && int128 } } } } */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 8 "forwprop1" { target { ! { oi_mode && int128 } } } } } */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 2 "forwprop2" } } */
/* { dg-final { scan-tree-dump-times "Long multiplication low part folded\\." 2 "forwprop1" } } */
/* Only the two 128-bit (OImode) chains are lowered.  sparc64 and hppa64
   have OImode and __int128 but no native DImode high part, so their u64
   chains lower too and the count would exceed 2; exclude them.  */
/* { dg-final { scan-tree-dump-times "Lowered long-mul high-part chain" 2 "widening_mul" { target { { oi_mode && int128 } && { ! { sparc*-*-* hppa*-*-* } } } } } } */
