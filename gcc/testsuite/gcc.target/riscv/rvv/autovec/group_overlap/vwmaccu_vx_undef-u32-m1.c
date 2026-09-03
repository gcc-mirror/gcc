/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned long long u64x4_t __attribute__ ((vector_size (32)));
typedef unsigned long long u64x2_t __attribute__ ((vector_size (16)));
typedef unsigned int       u32x4_t __attribute__ ((vector_size (16)));

u64x4_t
test_group_overlap_vwmaccu_vx_undef_u32 (u64x4_t acc, unsigned long long x)
{
  u64x2_t hi = __builtin_shufflevector (acc, acc, 2, 3);
  u32x4_t narrow = (u32x4_t) hi;
  u64x4_t widen = __builtin_convertvector (narrow, u64x4_t);
  unsigned long long nx = x & 0xffffffff;
  u64x4_t vx = { nx, nx, nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v2,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
