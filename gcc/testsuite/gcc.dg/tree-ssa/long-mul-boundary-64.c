/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O3" } */

typedef __UINT64_TYPE__ uint64_t;
typedef unsigned __int128 uint128_t;

/* Reference: high part of 64x64 -> 128 multiply.  */
__attribute__((noipa))
uint64_t mulh_ref (uint64_t x, uint64_t y)
{
  return (uint64_t)(((uint128_t)x * y) >> 64);
}

/* Carry pattern for high part.  */
__attribute__((noipa))
uint64_t mulh_carry (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t mulhilo = x_hi * y_lo;
  uint64_t mullohi = x_lo * y_hi;
  uint64_t cross_sum = mulhilo + mullohi;
  uint64_t mullolo = x_lo * y_lo;
  uint64_t shrlolo = mullolo >> 32;
  uint64_t add_cross_sum = cross_sum + shrlolo;
  int carry = add_cross_sum < mulhilo;
  uint64_t cond = ((uint64_t) carry << 32) + x_hi * y_hi;
  uint64_t add = cond + (add_cross_sum >> 32);

  return add;
}

/* Ladder pattern for high part.  */
__attribute__((noipa))
uint64_t mulh_ladder (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = y_lo * x_lo;
  uint64_t t1 = y_lo * x_hi;
  uint64_t t2 = y_hi * x_lo;
  uint64_t t3 = y_hi * x_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t u0 = t0_hi + t1;
  uint64_t u0_lo = u0 & 0xFFFFFFFFUL;
  uint64_t u0_hi = u0 >> 32;
  uint64_t u1 = u0_lo + t2;
  uint64_t u1_hi = u1 >> 32;
  uint64_t u2 = u0_hi + t3;
  uint64_t hw = u2 + u1_hi;

  return hw;
}

/* Ladder-long full multiply (both high and low parts).  */
__attribute__((noipa))
void full_mul (uint64_t x, uint64_t y, uint64_t *p)
{
  uint64_t xl = x & 0xFFFFFFFFUL;
  uint64_t xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFFUL;
  uint64_t yh = y >> 32;
  uint64_t mulll = xl * yl;
  uint64_t mullh = xl * yh;
  uint64_t mulhl = xh * yl;
  uint64_t mulhh = xh * yh;
  uint64_t shr8 = mulll >> 32;
  uint64_t conv10 = mullh & 0xFFFFFFFFUL;
  uint64_t add = shr8 + conv10;
  uint64_t conv12 = mulhl & 0xFFFFFFFFUL;
  uint64_t add13 = add + conv12;
  uint64_t shr14 = add13 >> 32;
  uint64_t shr15 = mullh >> 32;
  uint64_t add16 = mulhh + shr15;
  uint64_t shr17 = mulhl >> 32;
  uint64_t add18 = add16 + shr17;
  uint64_t add19 = add18 + shr14;
  p[1] = add19;
  uint64_t add_13_shl = add13 << 32;
  uint64_t and17 = mulll & 0xFFFFFFFFUL;
  uint64_t or_val = add_13_shl | and17;
  p[0] = or_val;
}

/* Two-carry pattern for high part.  */
__attribute__((noipa))
uint64_t mulh_two_carry (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;

  uint64_t lolo = x_lo * y_lo;
  uint64_t hilo = x_hi * y_lo;
  uint64_t lohi = x_lo * y_hi;
  uint64_t hihi = x_hi * y_hi;

  uint64_t cross_sum = hilo + lohi;
  uint64_t cross_carry = (uint64_t)(cross_sum < hilo) << 32;

  uint64_t cross_shifted = cross_sum << 32;
  uint64_t low_result = lolo + cross_shifted;
  uint64_t low_carry = (uint64_t)(low_result < cross_shifted);

  uint64_t high = hihi + (cross_sum >> 32) + cross_carry + low_carry;

  return high;
}

/* Two-carry full multiply (both high and low parts).  */
__attribute__((noipa))
void full_mul_two_carry (uint64_t x, uint64_t y, uint64_t *p)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;

  uint64_t lolo = x_lo * y_lo;
  uint64_t hilo = x_hi * y_lo;
  uint64_t lohi = x_lo * y_hi;
  uint64_t hihi = x_hi * y_hi;

  uint64_t cross_sum = hilo + lohi;
  uint64_t cross_carry = (uint64_t)(cross_sum < hilo) << 32;

  uint64_t cross_shifted = cross_sum << 32;
  uint64_t low_result = lolo + cross_shifted;
  uint64_t low_carry = (uint64_t)(low_result < cross_shifted);

  uint64_t high = hihi + (cross_sum >> 32) + cross_carry + low_carry;

  p[0] = low_result;
  p[1] = high;
}

/* Carry-long pattern for high part.  */
__attribute__((noipa))
uint64_t mulh_carry_long (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = y_lo * x_hi;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = y_hi * x_lo;
  uint64_t y_lo_x_lo = y_lo * x_lo;
  uint64_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = (cross_sum < y_lo_x_hi);
  uint64_t carry = (uint64_t) carry_out << 32;
  uint64_t y_lo_x_lo_hi = y_lo_x_lo >> 32;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFFUL;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  uint64_t interm = cross_sum_hi + y_hi_x_hi;
  uint64_t low_accum_hi = low_accum >> 32;
  uint64_t interm_plus_carry = interm + carry;
  return interm_plus_carry + low_accum_hi;
}

/* Carry-long full multiply (both high and low parts).  */
__attribute__((noipa))
void full_mul_carry_long (uint64_t x, uint64_t y, uint64_t *p)
{
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t x_hi = x >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo_x_hi = y_lo * x_hi;
  uint64_t y_hi_x_hi = y_hi * x_hi;
  uint64_t y_hi_x_lo = y_hi * x_lo;
  uint64_t y_lo_x_lo = y_lo * x_lo;
  uint64_t cross_sum = y_hi_x_lo + y_lo_x_hi;
  int carry_out = (cross_sum < y_lo_x_hi);
  uint64_t carry = (uint64_t) carry_out << 32;
  uint64_t y_lo_x_lo_hi = y_lo_x_lo >> 32;
  uint64_t cross_sum_lo = cross_sum & 0xFFFFFFFFUL;
  uint64_t cross_sum_hi = cross_sum >> 32;
  uint64_t low_accum = cross_sum_lo + y_lo_x_lo_hi;
  uint64_t upper_mid = y_hi_x_hi + carry;
  uint64_t low_accum_hi = low_accum >> 32;
  uint64_t upper_mid_with_cross = upper_mid + cross_sum_hi;
  p[1] = upper_mid_with_cross + low_accum_hi;
  uint64_t low_accum_shifted = low_accum << 32;
  uint64_t y_lo_x_lo_lo = y_lo_x_lo & 0xFFFFFFFFUL;
  p[0] = low_accum_shifted | y_lo_x_lo_lo;
}

/* Ladder-long pattern for high part.  */
__attribute__((noipa))
uint64_t mulh_ladder_long (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xFFFFFFFFUL;
  uint64_t xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFFUL;
  uint64_t yh = y >> 32;
  uint64_t mulll = xl * yl;
  uint64_t mullh = xl * yh;
  uint64_t mulhl = xh * yl;
  uint64_t mulhh = xh * yh;
  uint64_t shr8 = mulll >> 32;
  uint64_t conv10 = mullh & 0xFFFFFFFFUL;
  uint64_t add = shr8 + conv10;
  uint64_t conv12 = mulhl & 0xFFFFFFFFUL;
  uint64_t add13 = add + conv12;
  uint64_t shr14 = add13 >> 32;
  uint64_t shr15 = mullh >> 32;
  uint64_t add16 = mulhh + shr15;
  uint64_t shr17 = mulhl >> 32;
  uint64_t add18 = add16 + shr17;
  return add18 + shr14;
}

/* PHI-form, cond_carry_add (strict carry-on-true).  */
__attribute__((noipa))
uint64_t mulh_carry_phi (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t mulhilo = x_hi * y_lo;
  uint64_t mullohi = x_lo * y_hi;
  uint64_t cross_sum = mulhilo + mullohi;
  uint64_t mullolo = x_lo * y_lo;
  uint64_t add_cross_sum = cross_sum + (mullolo >> 32);
  uint64_t add = x_hi * y_hi + (add_cross_sum >> 32);
  if (add_cross_sum < mulhilo)
    add += (uint64_t)1 << 32;
  return add;
}

/* PHI-form, cond_carry_add_neg (negated branch, carry-on-false).  */
__attribute__((noipa))
uint64_t mulh_carry_phi_neg (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t mulhilo = x_hi * y_lo;
  uint64_t mullohi = x_lo * y_hi;
  uint64_t cross_sum = mulhilo + mullohi;
  uint64_t mullolo = x_lo * y_lo;
  uint64_t add_cross_sum = cross_sum + (mullolo >> 32);
  uint64_t add = x_hi * y_hi + (add_cross_sum >> 32);
  if (add_cross_sum >= mulhilo)
    ;
  else
    add += (uint64_t)1 << 32;
  return add;
}

/* PHI-form, cond_carry_add written as gt (operand-swapped from lt).  */
__attribute__((noipa))
uint64_t mulh_carry_phi_gt (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t mulhilo = x_hi * y_lo;
  uint64_t mullohi = x_lo * y_hi;
  uint64_t cross_sum = mulhilo + mullohi;
  uint64_t mullolo = x_lo * y_lo;
  uint64_t add_cross_sum = cross_sum + (mullolo >> 32);
  uint64_t add = x_hi * y_hi + (add_cross_sum >> 32);
  if (mulhilo > add_cross_sum)
    add += (uint64_t)1 << 32;
  return add;
}

/* PHI-form, cond_carry_add_neg written as le (operand-swapped from ge).  */
__attribute__((noipa))
uint64_t mulh_carry_phi_le (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t mulhilo = x_hi * y_lo;
  uint64_t mullohi = x_lo * y_hi;
  uint64_t cross_sum = mulhilo + mullohi;
  uint64_t mullolo = x_lo * y_lo;
  uint64_t add_cross_sum = cross_sum + (mullolo >> 32);
  uint64_t add = x_hi * y_hi + (add_cross_sum >> 32);
  if (mulhilo <= add_cross_sum)
    ;
  else
    add += (uint64_t)1 << 32;
  return add;
}

/* Low part via PLUS: lolo + (cross_sum << 32) with no comparison.  */
__attribute__((noipa))
uint64_t mul_low_plus (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;
  uint64_t lolo = x_lo * y_lo;
  uint64_t hilo = x_hi * y_lo;
  uint64_t lohi = x_lo * y_hi;
  uint64_t cross_sum = hilo + lohi;
  uint64_t cross_shifted = cross_sum << 32;
  return lolo + cross_shifted;
}

/* PHI-form, two-carry shape with the low carry as the PHI
   (LMK_CARRY_LOW path in match_long_mul_phi).  */
__attribute__((noipa))
uint64_t mulh_two_carry_low_phi (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;

  uint64_t lolo = x_lo * y_lo;
  uint64_t hilo = x_hi * y_lo;
  uint64_t lohi = x_lo * y_hi;
  uint64_t hihi = x_hi * y_hi;

  uint64_t cross_sum = hilo + lohi;
  uint64_t cross_carry = (uint64_t)(cross_sum < hilo) << 32;
  uint64_t cross_shifted = cross_sum << 32;
  uint64_t low_result = lolo + cross_shifted;

  uint64_t high = hihi + (cross_sum >> 32) + cross_carry;
  if (low_result < cross_shifted)
    high += 1;

  return high;
}

int main ()
{
  /* Boundary inputs: zero, one, half-word mask, half-word+1, signed max,
     unsigned max.  */
  uint64_t vals[] = {
    0, 1, 0xFFFFFFFFUL, 0x100000000ULL,
    0x7FFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL
  };
  int n = sizeof (vals) / sizeof (vals[0]);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      {
	uint64_t x = vals[i], y = vals[j];
	uint64_t expected_hi = mulh_ref (x, y);
	uint64_t expected_lo = x * y;

	if (mulh_carry (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_ladder (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_two_carry (x, y) != expected_hi)
	  __builtin_abort ();

	uint64_t p[2];
	full_mul (x, y, p);
	if (p[1] != expected_hi)
	  __builtin_abort ();
	if (p[0] != expected_lo)
	  __builtin_abort ();

	uint64_t q[2];
	full_mul_two_carry (x, y, q);
	if (q[1] != expected_hi)
	  __builtin_abort ();
	if (q[0] != expected_lo)
	  __builtin_abort ();

	if (mulh_carry_long (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_ladder_long (x, y) != expected_hi)
	  __builtin_abort ();

	uint64_t r[2];
	full_mul_carry_long (x, y, r);
	if (r[1] != expected_hi)
	  __builtin_abort ();
	if (r[0] != expected_lo)
	  __builtin_abort ();

	if (mulh_carry_phi (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_carry_phi_neg (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_carry_phi_gt (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_carry_phi_le (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_two_carry_low_phi (x, y) != expected_hi)
	  __builtin_abort ();

	if (mul_low_plus (x, y) != expected_lo)
	  __builtin_abort ();
      }

  return 0;
}
