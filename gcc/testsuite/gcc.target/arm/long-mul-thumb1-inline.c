/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-O2 -mthumb -mcpu=cortex-m0 -fdump-tree-forwprop1-details -fdump-tree-widening_mul-details" } */

/* Thumb-1 (cortex-m0) has no umull and no DImode multiply, so a DImode
   multiply would expand to the __aeabi_lmul libcall.  widening_mul
   re-synthesizes the recognized u32 high part from the HImode widening
   multiply Thumb-1 does have, so no libcall is emitted.  */

typedef __INT32_TYPE__ i32;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;

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

/* Signed operands sign-extended into the unsigned wide type.  The atom
   accepts them via `(convert? @X)'; long_mul_split_operand must sign-
   extend the narrow operand into the high half (arithmetic shift by
   N-1 on the signed narrow) -- a broken sign-extend branch would
   zero the high and miscompile any negative input.  */
u32 mulhs_split (i32 a, i32 b)
{
  return (u32) (((u64) a * (u64) b) >> 32);
}

/* Recognizer canonicalizes; widening_mul re-synthesizes the longhand.  */
/* { dg-final { scan-tree-dump "Long multiplication high part folded" "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "Lowered long-mul high-part chain" 2 "widening_mul" } } */
/* No multiplication libcall: the longhand stays inline.  */
/* { dg-final { scan-assembler-not "__aeabi_lmul" } } */
/* Split's signed branch emits an arithmetic shift by narrow_prec-1 on
   each signed narrow source of mulhs_split -- once per operand,
   absent when broken.  */
/* { dg-final { scan-tree-dump-times "\\(D\\) >> 31" 2 "widening_mul" } } */
