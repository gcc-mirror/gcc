/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef __UINT32_TYPE__ uint32_t;

/* Only one cross-product (xh*yl), missing xl*yh.
   Should NOT be folded.  */
uint32_t partial_one_cross (uint32_t x, uint32_t y)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_hi = y >> 16;
  uint32_t t0 = y_lo * x_lo;
  uint32_t t1 = y_lo * x_hi;
  uint32_t t3 = y_hi * x_hi;
  uint32_t t0_hi = t0 >> 16;
  uint32_t u0 = t0_hi + t1;
  uint32_t u0_hi = u0 >> 16;
  return t3 + u0_hi;
}

/* Only xl*yl and xh*yh, no cross-products at all.
   Should NOT be folded.  */
uint32_t partial_no_cross (uint32_t x, uint32_t y)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_hi = y >> 16;
  uint32_t t0 = y_lo * x_lo;
  uint32_t t3 = y_hi * x_hi;
  return t3 + (t0 >> 16);
}

/* Only cross-products, missing xl*yl and xh*yh.
   Should NOT be folded.  */
uint32_t partial_only_cross (uint32_t x, uint32_t y)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_hi = y >> 16;
  uint32_t t1 = y_lo * x_hi;
  uint32_t t2 = y_hi * x_lo;
  return (t1 + t2) >> 16;
}

/* Full ladder structure but one cross-product uses z instead of y.
   long_mul_check_consistency should reject the mismatched operand.
   Should NOT be folded.  */
uint32_t partial_mismatched_op (uint32_t x, uint32_t y, uint32_t z)
{
  uint32_t x_lo = x & 0xFFFF;
  uint32_t y_lo = y & 0xFFFF;
  uint32_t x_hi = x >> 16;
  uint32_t y_hi = y >> 16;
  uint32_t z_lo = z & 0xFFFF;
  uint32_t t0 = y_lo * x_lo;
  uint32_t t1 = y_lo * x_hi;
  uint32_t t2 = z_lo * x_lo;
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

/* Uses conditionals in the computation.
   Should NOT be folded.  */
unsigned mulhu_conditional (unsigned u, unsigned v) {
   unsigned a, b, c, d, p, q, rlow, rhigh;

   a = u >> 16;
   b = u & 0xFFFF;
   c = v >> 16;
   d = v & 0xFFFF;

   p = a*c;
   q = b*d;
   rlow = (-a + b)*(c - d);
   rhigh = (int)((-a + b)^(c - d)) >> 31;
   if (rlow == 0) rhigh = 0;

   q = q + (q >> 16);
   rlow = rlow + p;
   if (rlow < p) rhigh = rhigh + 1;
   rlow = rlow + q;
   if (rlow < q) rhigh = rhigh + 1;

   return p + (rlow >> 16) + (rhigh << 16);
}

/* Signed operands.
   Should NOT be folded.  */
int mulhs_signed (int u, int v) {
   unsigned u0, v0, w0;
   int u1, v1, w1, w2, t;

   u0 = u & 0xFFFF;
   u1 = u >> 16;
   v0 = v & 0xFFFF;
   v1 = v >> 16;
   w0 = u0*v0;
   t  = u1*v0 + (w0 >> 16);
   w1 = t & 0xFFFF;
   w2 = t >> 16;
   w1 = u0*v1 + w1;
   return u1*v1 + w2 + (w1 >> 16);
}

/* PHI-form near-miss: non-power-of-two carry increment.
   match.pd rejects via integer_pow2p@3.
   Should NOT be folded.  */
uint32_t partial_phi_nonpow2 (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16, x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16, y_lo = y & 0xFFFF;
  uint32_t mulhilo = x_hi * y_lo;
  uint32_t mullohi = x_lo * y_hi;
  uint32_t cross_sum = mulhilo + mullohi;
  uint32_t mullolo = x_lo * y_lo;
  uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
  uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
  if (add_cross_sum < mulhilo)
    add += (uint32_t)3 << 16;
  return add;
}

/* PHI-form near-miss: carry increment shifted by less than halfwidth.
   match_long_mul_phi rejects via shift_amt == halfwidth.
   Should NOT be folded.  */
uint32_t partial_phi_wrong_shift (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16, x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16, y_lo = y & 0xFFFF;
  uint32_t mulhilo = x_hi * y_lo;
  uint32_t mullohi = x_lo * y_hi;
  uint32_t cross_sum = mulhilo + mullohi;
  uint32_t mullolo = x_lo * y_lo;
  uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
  uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
  if (add_cross_sum < mulhilo)
    add += (uint32_t)1 << 15;
  return add;
}

/* PHI-form near-miss: equality compare in place of the strict carry
   predicate.  cond_carry_add and cond_carry_add_neg only encode
   gt / lt / le / ge.
   Should NOT be folded.  */
uint32_t partial_phi_wrong_compare (uint32_t x, uint32_t y)
{
  uint32_t x_hi = x >> 16, x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16, y_lo = y & 0xFFFF;
  uint32_t mulhilo = x_hi * y_lo;
  uint32_t mullohi = x_lo * y_hi;
  uint32_t cross_sum = mulhilo + mullohi;
  uint32_t mullolo = x_lo * y_lo;
  uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
  uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
  if (add_cross_sum == mulhilo)
    add += (uint32_t)1 << 16;
  return add;
}

/* PHI-form near-miss: one cross-product uses z instead of y.
   long_mul_check_consistency rejects the inconsistent (op0, op1).
   Should NOT be folded.  */
uint32_t partial_phi_wrong_cross (uint32_t x, uint32_t y, uint32_t z)
{
  uint32_t x_hi = x >> 16, x_lo = x & 0xFFFF;
  uint32_t y_hi = y >> 16, y_lo = y & 0xFFFF;
  uint32_t z_hi = z >> 16;
  uint32_t mulhilo = x_hi * y_lo;
  uint32_t mullohi = x_lo * z_hi;
  uint32_t cross_sum = mulhilo + mullohi;
  uint32_t mullolo = x_lo * y_lo;
  uint32_t add_cross_sum = cross_sum + (mullolo >> 16);
  uint32_t add = x_hi * y_hi + (add_cross_sum >> 16);
  if (add_cross_sum < mulhilo)
    add += (uint32_t)1 << 16;
  return add;
}

/* Verify no fold in any forwprop pass by checking the optimized IR
   for MULT_HIGHPART_EXPR (h*) and WIDEN_MULT_EXPR (w*).  */
/* { dg-final { scan-tree-dump-not " h\\* " "optimized" } } */
/* { dg-final { scan-tree-dump-not " w\\* " "optimized" } } */
