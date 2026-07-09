/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop-details -fdump-tree-widening_mul-details" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef struct { uint32_t v[2]; } v2i32;

/* High part follows the long form
   xh*yh + carry + (cross_sum >> N) + (low_accum >> N).  */

uint64_t mulh_carry (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = y_lo * x_hi;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = y_hi * x_lo;
  uint64_t y_lo_x_lo = y_lo * x_lo;
  uint64_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = cross_sum < y_lo_x_hi;
  uint64_t carry = (uint64_t) carry_out << 32;
  uint64_t y_lo_x_lo_hi = y_lo_x_lo >> 32;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFF;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  uint64_t interm = cross_sum_hi + y_hi_x_hi;
  uint64_t low_accum_hi = low_accum >> 32;
  uint64_t interm_plus_carry = interm + carry;
  uint64_t hw64 = interm_plus_carry + low_accum_hi;

  return hw64;
}

uint64_t mulh_carry_comm (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = x_hi * y_lo;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = x_lo * y_hi;
  uint64_t y_lo_x_lo = x_lo * y_lo;
  uint64_t cross_sum = y_lo_x_hi + y_hi_x_lo;
  int carry_out = (cross_sum < y_lo_x_hi);
  uint64_t carry = (uint64_t) carry_out << 32;
  uint64_t y_lo_x_lo_hi = y_lo_x_lo >> 32;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFF;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = y_lo_x_lo_hi + cross_sum_lo;
  uint64_t inter = y_hi_x_hi + cross_sum_hi;
  uint64_t low_accum_hi = low_accum >> 32;
  uint64_t interm_plus_carry = carry + inter;
  uint64_t hw64 = low_accum_hi + interm_plus_carry;

  return hw64;
}

uint32_t mulh_carry_32 (uint32_t x, uint32_t y)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo_x_hi = y_lo * x_hi;
  uint32_t y_hi_x_hi = y_hi * x_hi;
  uint32_t y_hi_x_lo = y_hi * x_lo;
  uint32_t y_lo_x_lo = y_lo * x_lo;
  uint32_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = (cross_sum < y_lo_x_hi);
  uint32_t carry = (uint32_t) carry_out << 16;
  uint32_t y_lo_x_lo_hi = y_lo_x_lo >> 16;
  uint32_t cross_sum_lo = cross_sum & 0xFFFF;
  uint32_t cross_sum_hi = cross_sum >> 16;
  uint32_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  uint32_t interm = cross_sum_hi + y_hi_x_hi;
  uint32_t low_accum_hi = low_accum >> 16;
  uint32_t interm_plus_carry = interm + carry;
  uint32_t hw64 = interm_plus_carry + low_accum_hi;

  return hw64;
}

/* The 128-bit variant lowers to longhand in pass_optimize_widening_mul;
   no target provides a 256-bit multiply.  */
#ifdef __SIZEOF_INT128__
__uint128_t mulh_carry_128 (__uint128_t x, __uint128_t y)
{
  __uint128_t x_lo = x & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t x_hi = x >> 64;
  __uint128_t y_lo = y & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t y_hi = y >> 64;
  __uint128_t y_lo_x_hi = y_lo * x_hi;
  __uint128_t y_hi_x_hi = y_hi * x_hi;
  __uint128_t y_hi_x_lo = y_hi * x_lo;
  __uint128_t y_lo_x_lo = y_lo * x_lo;
  __uint128_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = cross_sum < y_lo_x_hi;
  __uint128_t carry = (__uint128_t) carry_out << 64;
  __uint128_t y_lo_x_lo_hi = y_lo_x_lo >> 64;
  __uint128_t cross_sum_lo = cross_sum & (__uint128_t)0xFFFFFFFFFFFFFFFF;
  __uint128_t cross_sum_hi = cross_sum >> 64;
  __uint128_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  __uint128_t interm = cross_sum_hi + y_hi_x_hi;
  __uint128_t low_accum_hi = low_accum >> 64;
  __uint128_t interm_plus_carry = interm + carry;
  __uint128_t hw64 = interm_plus_carry + low_accum_hi;

  return hw64;
}
#endif

void full_mul_carry (uint64_t x, uint64_t y, uint64_t* p) {
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = y_lo * x_hi;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = y_hi * x_lo;
  uint64_t y_lo_x_lo = y_lo * x_lo;
  uint64_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = (cross_sum < y_lo_x_hi);
  uint64_t carry = (uint64_t) carry_out << 32;
  uint64_t y_lo_x_lo_hi = y_lo_x_lo >> 32;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFF;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  uint64_t upper_mid = y_hi_x_hi + carry;
  uint64_t low_accum_hi = low_accum >> 32;
  uint64_t upper_mid_with_cross = upper_mid + cross_sum_hi;
  uint64_t hw64 = upper_mid_with_cross + low_accum_hi;
  p[1] = hw64;
  uint64_t low_accum_shifted = low_accum << 32;
  uint64_t y_lo_x_lo_lo = y_lo_x_lo & 0xFFFFFFFF;
  uint64_t lw64 = low_accum_shifted | y_lo_x_lo_lo;
  p[0] = lw64;
}

/* This will be optimized during the second forwprop run.
   Disable SLP so the expected fold count is target-independent.  */
__attribute__((optimize("no-tree-slp-vectorize")))
v2i32 mulh_carry_v2i32 (v2i32 x, v2i32 y)
{
  v2i32 result;
  for (int i = 0; i < 2; i++)
    {
      uint32_t x_lo = x.v[i] & 0xFFFF;
      uint32_t y_lo = y.v[i] & 0xFFFF;
      uint32_t x_hi = x.v[i] >> 16;
      uint32_t y_hi = y.v[i] >> 16;

      uint32_t y_lo_x_hi = y_lo * x_hi;
      uint32_t y_hi_x_hi = y_hi * x_hi;
      uint32_t y_hi_x_lo = y_hi * x_lo;
      uint32_t y_lo_x_lo = y_lo * x_lo;

      uint32_t cross_sum = y_hi_x_lo + y_lo_x_hi;
      int carry_out = cross_sum < y_lo_x_hi;
      uint32_t carry = (uint32_t) carry_out << 16;

      uint32_t y_lo_x_lo_hi = y_lo_x_lo >> 16;
      uint32_t cross_sum_lo = cross_sum & 0xFFFF;
      uint32_t cross_sum_hi = cross_sum >> 16;

      uint32_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
      uint32_t interm = cross_sum_hi + y_hi_x_hi;
      uint32_t low_accum_hi = low_accum >> 16;
      uint32_t interm_plus_carry = interm + carry;

      result.v[i] = interm_plus_carry + low_accum_hi;
    }

  return result;
}

/* High part collapses the cross sum and (xl*yl >> N) into a single
   low_sum carrying the overflow compare:
     xh*yh + (low_sum >> N) + ((hilo > low_sum) << N).  */

uint32_t mulh_carry_low_sum (uint32_t x, uint32_t y)
{
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = x_hi * y_lo;
    uint32_t mullohi = x_lo * y_hi;
    uint32_t cross_sum = mulhilo + mullohi;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t shrlolo = mullolo >> 16;
    uint32_t low_sum = cross_sum + shrlolo;
    int carry = low_sum < mulhilo;
    uint32_t cond = ((uint32_t) carry << 16) + x_hi * y_hi;
    uint32_t add = cond + (low_sum >> 16);

    return add;
}

void full_mul_carry_low_sum (uint32_t x, uint32_t y, uint32_t* p)
{
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = x_hi * y_lo;
    uint32_t mullohi = x_lo * y_hi;
    uint32_t cross_sum = mulhilo + mullohi;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t shrlolo = mullolo >> 16;
    uint32_t low_sum = cross_sum + shrlolo;
    int carry = low_sum < mulhilo;
    uint32_t cond = ((uint32_t) carry << 16) + x_hi * y_hi;
    uint32_t add = cond + (low_sum >> 16);
    p[1] = add;
    uint32_t low_sum_shr = low_sum << 16;
    uint32_t mullololo = mullolo & 0xFFFF;
    uint32_t low = low_sum_shr | mullololo;
    p[0] = low;
}

uint32_t mulh_carry_low_sum_comm (uint32_t x, uint32_t y)
{
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = y_lo * x_hi;
    uint32_t mullohi = y_hi * x_lo;
    uint32_t cross_sum = mullohi + mulhilo;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t shrlolo = mullolo >> 16;
    uint32_t low_sum = shrlolo + cross_sum;
    int carry = low_sum < mulhilo;
    uint32_t cond = ((uint32_t) carry << 16) + x_hi * y_hi;
    uint32_t add = cond + (low_sum >> 16);

    return add;
}

uint32_t mulh_carry_low_sum_lohi (uint32_t x, uint32_t y) {
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = x_hi * y_lo;
    uint32_t mullohi = x_lo * y_hi;
    uint32_t cross_sum = mulhilo + mullohi;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t low_sum = cross_sum + (mullolo >> 16);
    int carry_occurred = (low_sum < mullohi);
    uint32_t cond = (uint32_t) carry_occurred << 16;
    uint32_t add = x_hi * y_hi + cond + (low_sum >> 16);

    return add;
}

/* The 128-bit variant will fail during the high sequence generation
   (no target provides a 256-bit multiply) and is excluded from the
   expected fold counts below.  */
#ifdef __SIZEOF_INT128__
__uint128_t mulh_carry_low_sum_128 (__uint128_t x, __uint128_t y)
{
    __uint128_t x_hi = x >> 64;
    __uint128_t x_lo = x & (__uint128_t)0xFFFFFFFFFFFFFFFF;
    __uint128_t y_hi = y >> 64;
    __uint128_t y_lo = y & (__uint128_t)0xFFFFFFFFFFFFFFFF;
    __uint128_t mulhilo = x_hi * y_lo;
    __uint128_t mullohi = x_lo * y_hi;
    __uint128_t cross_sum = mulhilo + mullohi;
    __uint128_t mullolo = x_lo * y_lo;
    __uint128_t shrlolo = mullolo >> 64;
    __uint128_t low_sum = cross_sum + shrlolo;
    int carry = low_sum < mulhilo;
    __uint128_t cond = ((__uint128_t) carry << 64) + x_hi * y_hi;
    __uint128_t add = cond + (low_sum >> 64);

    return add;
}
#endif

/* This will be optimized during the second forwprop run.
   Disable SLP so the expected fold count is target-independent.  */
__attribute__((optimize("no-tree-slp-vectorize")))
v2i32 mulh_carry_low_sum_v2i32 (v2i32 x, v2i32 y)
{
  v2i32 result;
  for (int i = 0; i < 2; i++)
    {
      uint32_t x_hi = x.v[i] >> 16;
      uint32_t x_lo = x.v[i] & 0xFFFF;
      uint32_t y_hi = y.v[i] >> 16;
      uint32_t y_lo = y.v[i] & 0xFFFF;
      uint32_t mulhilo  = x_hi * y_lo;
      uint32_t mullohi = x_lo * y_hi;
      uint32_t cross_sum  = mulhilo + mullohi;
      uint32_t mullolo = x_lo * y_lo;
      uint32_t shrlolo = mullolo >> 16;
      uint32_t low_sum = cross_sum + shrlolo;
      int carry = low_sum < mulhilo;
      uint32_t cond = ((uint32_t) carry << 16) + x_hi * y_hi;
      uint32_t add = cond + (low_sum >> 16);
      result.v[i] = add;
    }

  return result;
}

/* PHI-form coverage: the carry summand is the join of a 2-arg PHI
   guarded by an unsigned compare, recognized by cond_carry_add /
   cond_carry_add_neg and folded by match_long_mul_phi.  */

uint32_t mulh_carry_phi (uint32_t x, uint32_t y)
{
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = x_hi * y_lo;
    uint32_t mullohi = x_lo * y_hi;
    uint32_t cross_sum = mulhilo + mullohi;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
    uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
    if (add_cross_sum < mulhilo)
      add += (uint32_t)1 << 16;
    return add;
}

uint64_t mulh_carry_long_phi (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = y_lo * x_hi;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = y_hi * x_lo;
  uint64_t y_lo_x_lo = y_lo * x_lo;
  uint64_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFF;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = cross_sum_lo + (y_lo_x_lo >> 32);
  uint64_t hw64 = y_hi_x_hi + cross_sum_hi + (low_accum >> 32);
  if (cross_sum < y_lo_x_hi)
    hw64 += (uint64_t)1 << 32;
  return hw64;
}

/* PHI-form, cond_carry_add_neg (negated branch, carry-on-false).  */
uint32_t mulh_carry_phi_neg (uint32_t x, uint32_t y)
{
    uint32_t x_hi = x >> 16;
    uint32_t x_lo = x & 0xFFFF;
    uint32_t y_hi = y >> 16;
    uint32_t y_lo = y & 0xFFFF;
    uint32_t mulhilo = x_hi * y_lo;
    uint32_t mullohi = x_lo * y_hi;
    uint32_t cross_sum = mulhilo + mullohi;
    uint32_t mullolo = x_lo * y_lo;
    uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
    uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
    if (add_cross_sum >= mulhilo)
      ;
    else
      add += (uint32_t)1 << 16;
    return add;
}

/* On targets with __int128 support the two 128-bit highparts also
   fold; without it they are elided by #ifdef and the count drops
   by 2.  */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 10 "forwprop1" { target { oi_mode && int128 } } } } */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 8 "forwprop1" { target { ! { oi_mode && int128 } } } } } */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded\\." 2 "forwprop2" } } */
/* { dg-final { scan-tree-dump-times "Long multiplication low part folded\\." 2 "forwprop1" } } */
/* Three PHI-form highparts, one per polarity pair (gt via mulh_carry_phi
   and mulh_carry_long_phi, le via mulh_carry_phi_neg).  */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded \\(carry PHI\\)" 3 "forwprop1" } } */
/* Only the two 128-bit (OImode) chains are lowered.  sparc64 and hppa64
   have OImode and __int128 but no native DImode high part, so their u64
   chains lower too and the count would exceed 2; exclude them.  */
/* { dg-final { scan-tree-dump-times "Lowered long-mul high-part chain" 2 "widening_mul" { target { { oi_mode && int128 } && { ! { sparc*-*-* hppa*-*-* } } } } } } */
