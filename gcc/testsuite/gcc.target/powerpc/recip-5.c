/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -ftree-vectorize -mrecip=all -ffast-math -mdejagnu-cpu=power7 -fno-unroll-loops" } */
/* { dg-final { scan-assembler-times "xvredp" 4 } } */
/* { dg-final { scan-assembler-times "xvresp" 5 } } */
/* { dg-final { scan-assembler-times "xsredp\|fre\ " 2 } } */
/* { dg-final { scan-assembler-times "fres\|xsresp" 2 } } */
/* { dg-final { scan-assembler-times "fmuls\|xsmulsp" 2 } } */
/* { dg-final { scan-assembler-times "fnmsubs\|xsnmsub.sp" 2 } } */
/* { dg-final { scan-assembler-times "xsmuldp\|fmul\ " 2 } } */
/* { dg-final { scan-assembler-times "xsnmsub.dp\|fnmsub\ " 4 } } */
/* { dg-final { scan-assembler-times "xvmulsp" 7 } } */
/* { dg-final { scan-assembler-times "xvnmsub.sp" 5 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 6 } } */
/* { dg-final { scan-assembler-times "xvnmsub.dp" 8 } } */

#include <altivec.h>

float f_recip (float a, float b) { return __builtin_recipdivf (a, b); }
double d_recip (double a, double b) { return __builtin_recipdiv (a, b); }

float f_div (float a, float b) { return a / b; }
double d_div (double a, double b) { return a / b; }

#define SIZE 1024

double d_a[SIZE] __attribute__((__aligned__(32)));
double d_b[SIZE] __attribute__((__aligned__(32)));
double d_c[SIZE] __attribute__((__aligned__(32)));

float f_a[SIZE] __attribute__((__aligned__(32)));
float f_b[SIZE] __attribute__((__aligned__(32)));
float f_c[SIZE] __attribute__((__aligned__(32)));

void vec_f_recip (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f_a[i] = __builtin_recipdivf (f_b[i], f_c[i]);
}

void vec_d_recip (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d_a[i] = __builtin_recipdiv (d_b[i], d_c[i]);
}

void vec_f_div (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f_a[i] = f_b[i] / f_c[i];
}

void vec_f_div2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f_a[i] = f_b[i] / 2.0f;
}

void vec_f_div53 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f_a[i] = f_b[i] / 53.0f;
}

void vec_d_div (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d_a[i] = d_b[i] / d_c[i];
}

void vec_d_div2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d_a[i] = d_b[i] / 2.0;
}

void vec_d_div53 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d_a[i] = d_b[i] / 53.0;
}

vector float v4sf_recip1 (vector float a, vector float b) { return vec_recipdiv (a, b); }
vector float v4sf_recip2 (vector float a, vector float b) { return __builtin_altivec_vrecipdivfp (a, b); }
vector double v2df_recip1 (vector double a, vector double b) { return vec_recipdiv (a, b); }
vector float v4sf_recip3 (vector float a, vector float b) { return __builtin_vsx_xvrecipdivsp (a, b); }
vector double v2df_recip2 (vector double a, vector double b) { return __builtin_vsx_xvrecipdivdp (a, b); }
