/* { dg-do run } */
/* { dg-options "-O3" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* Reference: high part of 32x32 -> 64 multiply.  */
__attribute__((noipa))
uint32_t mulh_ref (uint32_t x, uint32_t y)
{
  return (uint32_t)(((uint64_t)x * y) >> 32);
}

/* Carry pattern for high part.  */
__attribute__((noipa))
uint32_t mulh_carry (uint32_t x, uint32_t y)
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
  uint32_t add_cross_sum = cross_sum + shrlolo;
  int carry = add_cross_sum < mulhilo;
  uint32_t cond = ((uint32_t) carry << 16) + x_hi * y_hi;
  uint32_t add = cond + (add_cross_sum >> 16);

  return add;
}

/* Ladder pattern for high part.  */
__attribute__((noipa))
uint32_t mulh_ladder (uint32_t x, uint32_t y)
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
  uint32_t hw = u2 + u1_hi;

  return hw;
}

/* Ladder-long pattern for full multiplication (both high and low parts).  */
__attribute__((noipa))
void full_mul (uint32_t x, uint32_t y, uint32_t *p)
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

/* Two-carry pattern for high part (32-bit).  */
__attribute__((noipa))
uint32_t mulh_two_carry (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16;
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo = y & 0xFFFF;

  uint32_t lolo = x_lo * y_lo;
  uint32_t hilo = x_hi * y_lo;
  uint32_t lohi = x_lo * y_hi;
  uint32_t hihi = x_hi * y_hi;

  uint32_t cross_sum = hilo + lohi;
  uint32_t cross_carry = (uint32_t)(cross_sum < hilo) << 16;

  uint32_t cross_shifted = cross_sum << 16;
  uint32_t low_result = lolo + cross_shifted;
  uint32_t low_carry = (uint32_t)(low_result < cross_shifted);

  uint32_t high = hihi + (cross_sum >> 16) + cross_carry + low_carry;

  return high;
}

/* Two-carry full multiply (32-bit, both high and low parts).  */
__attribute__((noipa))
void full_mul_two_carry (uint32_t x, uint32_t y, uint32_t *p)
{
  uint32_t x_hi = x >> 16;
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo = y & 0xFFFF;

  uint32_t lolo = x_lo * y_lo;
  uint32_t hilo = x_hi * y_lo;
  uint32_t lohi = x_lo * y_hi;
  uint32_t hihi = x_hi * y_hi;

  uint32_t cross_sum = hilo + lohi;
  uint32_t cross_carry = (uint32_t)(cross_sum < hilo) << 16;

  uint32_t cross_shifted = cross_sum << 16;
  uint32_t low_result = lolo + cross_shifted;
  uint32_t low_carry = (uint32_t)(low_result < cross_shifted);

  uint32_t high = hihi + (cross_sum >> 16) + cross_carry + low_carry;

  p[0] = low_result;
  p[1] = high;
}

/* Carry-long pattern for high part.  */
__attribute__((noipa))
uint32_t mulh_carry_long (uint32_t x, uint32_t y)
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
  return interm_plus_carry + low_accum_hi;
}

/* Carry-long full multiply (both high and low parts).  */
__attribute__((noipa))
void full_mul_carry_long (uint32_t x, uint32_t y, uint32_t *p)
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
  uint32_t upper_mid = y_hi_x_hi + carry;
  uint32_t low_accum_hi = low_accum >> 16;
  uint32_t upper_mid_with_cross = upper_mid + cross_sum_hi;
  p[1] = upper_mid_with_cross + low_accum_hi;
  uint32_t low_accum_shifted = low_accum << 16;
  uint32_t y_lo_x_lo_lo = y_lo_x_lo & 0xFFFF;
  p[0] = low_accum_shifted | y_lo_x_lo_lo;
}

/* Ladder-long pattern for high part.  */
__attribute__((noipa))
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
  return add18 + shr14;
}

/* PHI-form, cond_carry_add (strict carry-on-true).  */
__attribute__((noipa))
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

/* PHI-form, cond_carry_add_neg (negated branch, carry-on-false).  */
__attribute__((noipa))
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

/* PHI-form, cond_carry_add written as gt (operand-swapped from lt).  */
__attribute__((noipa))
uint32_t mulh_carry_phi_gt (uint32_t x, uint32_t y)
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
  if (mulhilo > add_cross_sum)
    add += (uint32_t)1 << 16;
  return add;
}

/* PHI-form, cond_carry_add_neg written as le (operand-swapped from ge).  */
__attribute__((noipa))
uint32_t mulh_carry_phi_le (uint32_t x, uint32_t y)
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
  if (mulhilo <= add_cross_sum)
    ;
  else
    add += (uint32_t)1 << 16;
  return add;
}

/* PHI-form, two-carry shape with the low carry as the PHI
   (LMK_CARRY_LOW path in match_long_mul_phi).  */
__attribute__((noipa))
uint32_t mulh_two_carry_low_phi (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16;
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo = y & 0xFFFF;

  uint32_t lolo = x_lo * y_lo;
  uint32_t hilo = x_hi * y_lo;
  uint32_t lohi = x_lo * y_hi;
  uint32_t hihi = x_hi * y_hi;

  uint32_t cross_sum = hilo + lohi;
  uint32_t cross_carry = (uint32_t)(cross_sum < hilo) << 16;
  uint32_t cross_shifted = cross_sum << 16;
  uint32_t low_result = lolo + cross_shifted;

  uint32_t high = hihi + (cross_sum >> 16) + cross_carry;
  if (low_result < cross_shifted)
    high += 1;

  return high;
}

int main ()
{
  /* Boundary inputs: zero, one, half-word mask, half-word+1, signed max,
     unsigned max.  */
  uint32_t vals[] = { 0, 1, 0xFFFF, 0x10000, 0x7FFFFFFFU, 0xFFFFFFFFU };
  int n = sizeof (vals) / sizeof (vals[0]);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      {
	uint32_t x = vals[i], y = vals[j];
	uint32_t expected_hi = mulh_ref (x, y);
	uint32_t expected_lo = x * y;

	if (mulh_carry (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_ladder (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_two_carry (x, y) != expected_hi)
	  __builtin_abort ();

	uint32_t p[2];
	full_mul (x, y, p);
	if (p[1] != expected_hi)
	  __builtin_abort ();
	if (p[0] != expected_lo)
	  __builtin_abort ();

	uint32_t q[2];
	full_mul_two_carry (x, y, q);
	if (q[1] != expected_hi)
	  __builtin_abort ();
	if (q[0] != expected_lo)
	  __builtin_abort ();

	if (mulh_carry_long (x, y) != expected_hi)
	  __builtin_abort ();

	if (mulh_ladder_long (x, y) != expected_hi)
	  __builtin_abort ();

	uint32_t r[2];
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
      }

  return 0;
}
