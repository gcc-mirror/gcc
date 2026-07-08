/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -fexpensive-optimizations" } */

/* The -Og pipeline does not run pass_optimize_widening_mul, so the
   long-multiply fold must not emit a 2N wide-multiply chain here:
   nothing would lower it before expansion.  */

typedef __uint128_t u128;

u128
mulh (u128 x, u128 y)
{
  u128 x_hi = x >> 64;
  u128 x_lo = x & (u128)0xFFFFFFFFFFFFFFFF;
  u128 y_hi = y >> 64;
  u128 y_lo = y & (u128)0xFFFFFFFFFFFFFFFF;
  u128 mulhilo = x_hi * y_lo;
  u128 mullohi = x_lo * y_hi;
  u128 cross_sum = mulhilo + mullohi;
  u128 mullolo = x_lo * y_lo;
  u128 shrlolo = mullolo >> 64;
  u128 add_cross_sum = cross_sum + shrlolo;
  int carry = add_cross_sum < mulhilo;
  u128 cond = ((u128) carry << 64) + x_hi * y_hi;
  return cond + (add_cross_sum >> 64);
}
