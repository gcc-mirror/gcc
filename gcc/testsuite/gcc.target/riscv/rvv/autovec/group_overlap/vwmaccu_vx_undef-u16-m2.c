/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned int   u32x16_t __attribute__ ((vector_size (64)));
typedef unsigned int   u32x8_t  __attribute__ ((vector_size (32)));
typedef unsigned short u16x16_t __attribute__ ((vector_size (32)));

u32x16_t
test_group_overlap_vwmaccu_vx_undef_u16 (u32x16_t acc, unsigned int x)
{
  u32x8_t hi = __builtin_shufflevector (acc, acc, 8, 9, 10, 11, 12, 13, 14, 15);
  u16x16_t narrow = (u16x16_t) hi;
  u32x16_t widen = __builtin_convertvector (narrow, u32x16_t);
  unsigned int nx = x & 0xffff;
  u32x16_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
