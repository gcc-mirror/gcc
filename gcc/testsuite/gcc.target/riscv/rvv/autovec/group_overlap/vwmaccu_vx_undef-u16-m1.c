/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned int   u32x8_t __attribute__ ((vector_size (32)));
typedef unsigned int   u32x4_t __attribute__ ((vector_size (16)));
typedef unsigned short u16x8_t __attribute__ ((vector_size (16)));

u32x8_t
test_group_overlap_vwmaccu_vx_undef_u16 (u32x8_t acc, unsigned int x)
{
  u32x4_t hi = __builtin_shufflevector (acc, acc, 4, 5, 6, 7);
  u16x8_t narrow = (u16x8_t) hi;
  u32x8_t widen = __builtin_convertvector (narrow, u32x8_t);
  unsigned int nx = x & 0xffff;
  u32x8_t vx = { nx, nx, nx, nx, nx, nx, nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v2,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
