/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned long long u64x8_t __attribute__ ((vector_size (64)));
typedef unsigned long long u64x4_t __attribute__ ((vector_size (32)));
typedef unsigned int       u32x8_t __attribute__ ((vector_size (32)));

u64x8_t
test_group_overlap_vwmaccu_vx_undef_u32 (u64x8_t acc, unsigned long long x)
{
  u64x4_t hi = __builtin_shufflevector (acc, acc, 4, 5, 6, 7);
  u32x8_t narrow = (u32x8_t) hi;
  u64x8_t widen = __builtin_convertvector (narrow, u64x8_t);
  unsigned long long nx = x & 0xffffffff;
  u64x8_t vx = { nx, nx, nx, nx, nx, nx, nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
