/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned long long u64x16_t __attribute__ ((vector_size (128)));
typedef unsigned long long u64x8_t  __attribute__ ((vector_size (64)));
typedef unsigned int       u32x16_t __attribute__ ((vector_size (64)));

u64x16_t
test_group_overlap_vwmaccu_vx_undef_u32 (u64x16_t acc, unsigned long long x)
{
  u64x8_t hi = __builtin_shufflevector (acc, acc, 8, 9, 10, 11, 12, 13, 14, 15);
  u32x16_t narrow = (u32x16_t) hi;
  u64x16_t widen = __builtin_convertvector (narrow, u64x16_t);
  unsigned long long nx = x & 0xffffffff;
  u64x16_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
