/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned int   u32x2_t __attribute__ ((vector_size (8)));
typedef unsigned short u16x2_t __attribute__ ((vector_size (4)));

u32x2_t
test_group_overlap_vwmaccu_vx_undef_u16 (u32x2_t acc, u16x2_t narrow,
                                         unsigned int x)
{
  u32x2_t widen = __builtin_convertvector (narrow, u32x2_t);
  unsigned int nx = x & 0xffff;
  u32x2_t vx = { nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
