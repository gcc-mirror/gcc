/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned short u16x16_t __attribute__ ((vector_size (32)));
typedef unsigned short u16x8_t  __attribute__ ((vector_size (16)));
typedef unsigned char  u8x16_t  __attribute__ ((vector_size (16)));

u16x16_t
test_group_overlap_vwmaccu_vx_undef_u8 (u16x16_t acc, unsigned short x)
{
  u16x8_t hi = __builtin_shufflevector (acc, acc, 8, 9, 10, 11, 12, 13, 14, 15);
  u8x16_t narrow = (u8x16_t) hi;
  u16x16_t widen = __builtin_convertvector (narrow, u16x16_t);
  unsigned short nx = x & 0xff;
  u16x16_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v2,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
