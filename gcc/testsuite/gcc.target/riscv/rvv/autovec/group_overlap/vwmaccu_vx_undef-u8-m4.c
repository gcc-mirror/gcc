/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

typedef unsigned short u16x64_t __attribute__ ((vector_size (128)));
typedef unsigned short u16x32_t __attribute__ ((vector_size (64)));
typedef unsigned char  u8x64_t  __attribute__ ((vector_size (64)));

u16x64_t
test_group_overlap_vwmaccu_vx_undef_u8 (u16x64_t acc, unsigned short x)
{
  u16x32_t hi = __builtin_shufflevector (
    acc, acc,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63);
  u8x64_t narrow = (u8x64_t) hi;
  u16x64_t widen = __builtin_convertvector (narrow, u16x64_t);
  unsigned short nx = x & 0xff;
  u16x64_t vx = {
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx,
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx,
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx,
    nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx, nx
  };

  return acc + widen * vx;
}

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
