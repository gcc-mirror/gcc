/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3" } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* 64-bit ladder pattern for high part.  */
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
  return u2 + u1_hi;
}

/* 64-bit carry pattern for high part.  */
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
  return interm_plus_carry + low_accum_hi;
}

/* 32-bit ladder pattern for high part.  */
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
  return u2 + u1_hi;
}

/* 32-bit carry pattern for high part.  */
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
  return interm_plus_carry + low_accum_hi;
}

/* 64-bit patterns should emit mulq (unsigned 64x64->128 multiply).  */
/* { dg-final { scan-assembler-times "\tmulq" 2 } } */
/* 32-bit patterns should emit imulq (64-bit multiply of zero-extended operands).  */
/* { dg-final { scan-assembler-times "\timulq" 2 } } */
