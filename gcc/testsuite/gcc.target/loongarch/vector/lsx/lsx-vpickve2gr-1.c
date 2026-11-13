/* { dg-do compile } */
/* { dg-options "-mlsx -O3" } */
#include <lsxintrin.h>
extern int *x_si0;
extern int *x_si1;
extern long long *x_di0;
extern long long *x_di1;
/* { dg-final { scan-assembler-times "i32_0:.*\tmovfr2gr\\.s.*i32_0" 1 } } */
int
lsx_pickve2gr_i32_0 ()
{
  v4i32 a = { x_si0[0], x_si0[1], x_si0[2], x_si0[3] };
  v4i32 b = { x_si1[0], x_si1[1], x_si1[2], x_si1[3] };
  v4i32 res = a + b;
  return res[0];
}
/* { dg-final { scan-assembler-times "i32_1:.*\tmovfrh2gr\\.s.*i32_1" 1 } } */
int
lsx_pickve2gr_i32_1 ()
{
  v4i32 a = { x_si0[0], x_si0[1], x_si0[2], x_si0[3] };
  v4i32 b = { x_si1[0], x_si1[1], x_si1[2], x_si1[3] };
  v4i32 res = a + b;
  return res[1];
}
/* { dg-final { scan-assembler-times "i64:.*\tmovfr2gr\\.d.*i64" 1 } } */
long long
lsx_pickve2gr_i64 ()
{
  v2i64 a = { x_di0[0], x_di0[1] };
  v2i64 b = { x_di1[0], x_di1[1] };
  v2i64 res = a + b;
  return res[0];
}
