/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \t\]+\[^\n\]*%xmm\[0-9\]" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfmsubadd...ph\[ \t\]+\[^\n\]*%xmm\[0-9\]" 1 { target { ! ia32 } } } } */

void vec_fmaddsub_fp16(int n, _Float16 da_r, _Float16 *x, _Float16* y, _Float16* __restrict z)
{
  for (int i = 0; i < 4; i += 2)
    {
      z[i] =  da_r * x[i] - y[i];
      z[i+1]  =  da_r * x[i+1] + y[i+1];
    }
}

void vec_fmasubadd_fp16(int n, _Float16 da_r, _Float16 *x, _Float16* y, _Float16* __restrict z)
{
  for (int i = 0; i < 4; i += 2)
    {
      z[i] =  da_r * x[i] + y[i];
      z[i+1]  =  da_r * x[i+1] - y[i+1];
    }
}
