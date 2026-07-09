/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-O2 -marm -fdump-tree-widening_mul-details" } */

/* With umull available, the recognizer folds the u32 high-part longhand
   to a DImode multiply that becomes a single umull, while the u64
   longhand becomes a TImode chain that widening_mul re-synthesizes as
   four umull.  No libcalls.  */

typedef __UINT64_TYPE__ u64;
typedef __UINT32_TYPE__ u32;

u32 mulh32 (u32 x, u32 y)
{
  u32 x_hi = x >> 16, x_lo = x & 0xFFFF;
  u32 y_hi = y >> 16, y_lo = y & 0xFFFF;
  u32 mulhilo = x_hi * y_lo;
  u32 mullohi = x_lo * y_hi;
  u32 cross_sum = mulhilo + mullohi;
  u32 mullolo = x_lo * y_lo;
  u32 shrlolo = mullolo >> 16;
  u32 acs = cross_sum + shrlolo;
  int carry = acs < mulhilo;
  u32 cond = ((u32) carry << 16) + x_hi * y_hi;
  return cond + (acs >> 16);
}

u64 mulh64 (u64 x, u64 y)
{
  u64 x_hi = x >> 32, x_lo = x & 0xFFFFFFFF;
  u64 y_hi = y >> 32, y_lo = y & 0xFFFFFFFF;
  u64 mulhilo = x_hi * y_lo;
  u64 mullohi = x_lo * y_hi;
  u64 cross_sum = mulhilo + mullohi;
  u64 mullolo = x_lo * y_lo;
  u64 shrlolo = mullolo >> 32;
  u64 acs = cross_sum + shrlolo;
  int carry = acs < mulhilo;
  u64 cond = ((u64) carry << 32) + x_hi * y_hi;
  return cond + (acs >> 32);
}

/* Two longhands chained through the low half, with the first high part also
   live.  After recognition the second chain reads its operand off the first
   product as a masked low half, so lowering both leaves a live TImode
   multiply the target cannot expand.  */

u64 chain_low (u64 a, u64 b, u64 c, u64 *hi1)
{
  *hi1 = mulh64 (a, b);
  return mulh64 (a * b, c);
}

/* The same through the high half: after recognition the second chain
   multiplies by the first product shifted down, and truncating that shift
   would leave the product live instead.  */

u64 chain_high (u64 a, u64 b, u64 c, u64 *hi1)
{
  u64 h1 = mulh64 (a, b);
  *hi1 = h1;
  return mulh64 (h1, c);
}

/* mulh32 collapses to one umull and mulh64 lowers to four; chain_low and
   chain_high inline two longhands each, for nine and eight.  */
/* { dg-final { scan-assembler-times "\tumull\t" 22 } } */
/* One lowering in mulh64 and two in each chain.  mulh32 contributes none:
   its DImode multiply is converted to a widening multiply instead.  */
/* { dg-final { scan-tree-dump-times "Lowered long-mul high-part chain" 5 "widening_mul" } } */
/* { dg-final { scan-tree-dump "Narrowed low-half-only long multiply" "widening_mul" } } */
/* { dg-final { scan-assembler-not "__aeabi_lmul" } } */
/* { dg-final { scan-assembler-not "__multi3" } } */
