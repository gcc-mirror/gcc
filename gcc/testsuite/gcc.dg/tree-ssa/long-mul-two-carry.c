/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop-details" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* High part using two separate carries (cross carry + low carry).  */
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

/* Commuted operand order.  */
uint64_t mulh_two_carry_comm (uint64_t x, uint64_t y)
{
  uint64_t x_hi = x >> 32;
  uint64_t x_lo = x & 0xFFFFFFFFUL;
  uint64_t y_hi = y >> 32;
  uint64_t y_lo = y & 0xFFFFFFFFUL;

  uint64_t lolo = x_lo * y_lo;
  uint64_t lohi = x_lo * y_hi;
  uint64_t hilo = x_hi * y_lo;
  uint64_t hihi = x_hi * y_hi;

  uint64_t cross_sum = lohi + hilo;
  uint64_t cross_carry = (uint64_t)(cross_sum < lohi) << 32;

  uint64_t cross_shifted = cross_sum << 32;
  uint64_t low_result = cross_shifted + lolo;
  uint64_t low_carry = (uint64_t)(low_result < lolo);

  uint64_t high = hihi + (cross_sum >> 32) + cross_carry + low_carry;

  return high;
}

/* 32-bit variant.  */
uint32_t mulh_two_carry_32 (uint32_t x, uint32_t y)
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

/* Full multiply: both high and low parts.  */
uint64_t full_mul_two_carry (uint64_t x, uint64_t y, uint64_t *lo)
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

  *lo = low_result;
  return high;
}

/* Folds land in forwprop3 because the LT_EXPR low-carry compare is
   only canonicalized into the gt:c-plus-overflow shape that
   mul_carry_low matches by an earlier pass between forwprop1 and
   forwprop3.  */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded." 4 "forwprop3" } } */