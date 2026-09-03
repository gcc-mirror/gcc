/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned short u16x8_t __attribute__ ((vector_size (16)));
typedef unsigned char  u8x8_t  __attribute__ ((vector_size (8)));

u16x8_t
test_group_overlap_vwmaccu_vx_undef_u8 (u16x8_t acc, u8x8_t narrow,
                                        unsigned short x)
{
  u16x8_t widen = __builtin_convertvector (narrow, u16x8_t);
  unsigned short nx = x & 0xff;
  u16x8_t vx = { nx, nx, nx, nx, nx, nx, nx, nx };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
