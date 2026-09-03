/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned int   u32x32_t __attribute__ ((vector_size (128)));
typedef unsigned int   u32x16_t __attribute__ ((vector_size (64)));
typedef unsigned short u16x32_t __attribute__ ((vector_size (64)));

u32x32_t
test_group_overlap_vwmaccu_vx_undef_u16 (u32x32_t acc, unsigned int x)
{
  u32x16_t hi = __builtin_shufflevector (
    acc, acc,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31);
  u16x32_t narrow = (u16x32_t) hi;
  u32x32_t widen = __builtin_convertvector (narrow, u32x32_t);
  unsigned int nx = x & 0xffff;
  u32x32_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx,
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
