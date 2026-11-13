
/* { dg-do compile } */
/* { dg-options "-mlasx -O3" } */
#include <lasxintrin.h>
extern int *x_si0;
extern int *x_si1;
extern long long *x_di0;
extern long long *x_di1;
/* { dg-final { scan-assembler-times "i32_0:.*\tmovfr2gr\\.s.*i32_0" 1 } } */
int
lasx_pickve2gr_i32_0 ()
{
  v8i32 a = { x_si0[0], x_si0[1], x_si0[2], x_si0[3],
              x_si0[4], x_si0[5], x_si0[6], x_si0[7] };
  v8i32 b = { x_si1[0], x_si1[1], x_si1[2], x_si1[3],
              x_si1[4], x_si1[5], x_si1[6], x_si1[7] };
  v8i32 res = a + b;
  return res[0];
}
/* { dg-final { scan-assembler-times "i32_1:.*\tmovfrh2gr\\.s.*i32_1" 1 } } */
int
lasx_pickve2gr_i32_1 ()
{
  v8i32 a = { x_si0[0], x_si0[1], x_si0[2], x_si0[3],
              x_si0[4], x_si0[5], x_si0[6], x_si0[7] };
  v8i32 b = { x_si1[0], x_si1[1], x_si1[2], x_si1[3],
              x_si1[4], x_si1[5], x_si1[6], x_si1[7] };
  v8i32 res = a + b;
  return res[1];
}
/* { dg-final { scan-assembler-times "i32_2:.*\tvpickve2gr\\.w.*i32_2" 1 } } */
int
lasx_pickve2gr_i32_2 ()
{
  v8i32 a = { x_si0[0], x_si0[1], x_si0[2], x_si0[3],
              x_si0[4], x_si0[5], x_si0[6], x_si0[7] };
  v8i32 b = { x_si1[0], x_si1[1], x_si1[2], x_si1[3],
              x_si1[4], x_si1[5], x_si1[6], x_si1[7] };
  v8i32 res = a + b;
  return res[2];
}
/* { dg-final { scan-assembler-times "i64_0:.*\tmovfr2gr\\.d.*i64_0" 1 } } */
long long
lasx_pickve2gr_i64_0 ()
{
  v4i64 a = { x_di0[0], x_di0[1], x_di0[2], x_di0[3] };
  v4i64 b = { x_di1[0], x_di1[1], x_di1[2], x_di1[3] };
  v4i64 res = a + b;
  return res[0];
}
