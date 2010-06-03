/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O3 -ftree-vectorize -mrecip -ffast-math -mcpu=power7 -fno-unroll-loops" } */
/* { dg-final { scan-assembler-times "xvrsqrtedp" 1 } } */
/* { dg-final { scan-assembler-times "xvmsub.dp" 1 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 4 } } */
/* { dg-final { scan-assembler-times "xvnmsub.dp" 2 } } */
/* { dg-final { scan-assembler-times "xvrsqrtesp" 1 } } */
/* { dg-final { scan-assembler-times "xvmsub.sp" 1 } } */
/* { dg-final { scan-assembler-times "xvmulsp" 4 } } */
/* { dg-final { scan-assembler-times "xvnmsub.sp" 2 } } */

#define SIZE 1024

extern double a_d[SIZE] __attribute__((__aligned__(32)));
extern double b_d[SIZE] __attribute__((__aligned__(32)));

void
vectorize_rsqrt_d (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a_d[i] = 1.0 / __builtin_sqrt (b_d[i]);
}

extern float a_f[SIZE] __attribute__((__aligned__(32)));
extern float b_f[SIZE] __attribute__((__aligned__(32)));

void
vectorize_rsqrt_f (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a_f[i] = 1.0f / __builtin_sqrtf (b_f[i]);
}
