/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned short u16x2_t __attribute__ ((vector_size (4)));
typedef unsigned char  u8x2_t  __attribute__ ((vector_size (2)));

u16x2_t
test_group_overlap_vwmaccu_vx_undef_u8 (u16x2_t acc, u8x2_t narrow,
                                        unsigned short x)
{
  u16x2_t widen = __builtin_convertvector (narrow, u16x2_t);
  unsigned short nx = x & 0xff;
  u16x2_t vx = { nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
