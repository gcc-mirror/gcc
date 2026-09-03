/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned int   u32x4_t __attribute__ ((vector_size (16)));
typedef unsigned short u16x4_t __attribute__ ((vector_size (8)));

u32x4_t
test_group_overlap_vwmaccu_vx_undef_u16 (u32x4_t acc, u16x4_t narrow,
                                         unsigned int x)
{
  u32x4_t widen = __builtin_convertvector (narrow, u32x4_t);
  unsigned int nx = x & 0xffff;
  u32x4_t vx = { nx, nx, nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
