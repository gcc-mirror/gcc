/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned short u16x32_t __attribute__ ((vector_size (64)));
typedef unsigned short u16x16_t __attribute__ ((vector_size (32)));
typedef unsigned char  u8x32_t  __attribute__ ((vector_size (32)));

u16x32_t
test_group_overlap_vwmaccu_vx_undef_u8 (u16x32_t acc, unsigned short x)
{
  u16x16_t hi = __builtin_shufflevector (
    acc, acc,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31);
  u8x32_t narrow = (u8x32_t) hi;
  u16x32_t widen = __builtin_convertvector (narrow, u16x32_t);
  unsigned short nx = x & 0xff;
  u16x32_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx,
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
