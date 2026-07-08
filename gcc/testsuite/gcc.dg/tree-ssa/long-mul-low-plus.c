/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop-details" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* Low part via PLUS form: lolo + (cross_sum << halfwidth).
   No GT/LT comparison on the result, so long_mul_check_low_plus_defer
   should fold without deferring.  */
uint32_t mul_low_plus_32 (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16;
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t lolo = x_lo * y_lo;
  uint32_t hilo = x_hi * y_lo;
  uint32_t lohi = x_lo * y_hi;
  uint32_t cross_sum = hilo + lohi;
  uint32_t cross_shifted = cross_sum << 16;
  return lolo + cross_shifted;
}

/* 64-bit variant.  */
uint64_t mul_low_plus_64 (uint64_t x, uint64_t y)
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

/* Commuted operand order.  */
uint32_t mul_low_plus_comm (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16;
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t lolo = y_lo * x_lo;
  uint32_t hilo = y_lo * x_hi;
  uint32_t lohi = y_hi * x_lo;
  uint32_t cross_sum = lohi + hilo;
  uint32_t cross_shifted = cross_sum << 16;
  return cross_shifted + lolo;
}

/* { dg-final { scan-tree-dump-times "Long multiplication low part folded." 3 "forwprop1" } } */
