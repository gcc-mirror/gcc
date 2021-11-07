/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -mfpmath=sse -fdump-tree-optimized" } */

#include<math.h>
void foo1 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  for (int i = 0; i != 8; i++)
    a[i] = copysignf (b[i], c[i]);
}

void foo2 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  for (int i = 0; i != 8; i++)
    a[i] = copysign (b[i], c[i]);
}

void foo3 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  for (int i = 0; i != 8; i++)
    a[i] = copysignl (b[i], c[i]);
}

void foo4 (float* __restrict a, float* b, float* c)
{
  for (int i = 0; i != 4; i++)
    a[i] = copysign (b[i], c[i]);
}

void foo5 (float* __restrict a, float* b, float* c)
{
  for (int i = 0; i != 4; i++)
    a[i] = copysignl (b[i], c[i]);
}

void foo6 (double* __restrict a, double* b, double* c)
{
  for (int i = 0; i != 4; i++)
    a[i] = copysignl (b[i], c[i]);
}

void foo7 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  a[0] = copysignf (b[0], c[0]);
}

void foo8 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  a[0] = copysign (b[0], c[0]);
}

void foo9 (_Float16* __restrict a, _Float16* b, _Float16* c)
{
  a[0] = copysignl (b[0], c[0]);
}

void foo10 (float* __restrict a, float* b, float* c)
{
  a[0] = copysign (b[0], c[0]);
}

void foo11 (float* __restrict a, float* b, float* c)
{
  a[0] = copysignl (b[0], c[0]);
}

void foo12 (double* __restrict a, double* b, double* c)
{
  a[0] = copysignl (b[0], c[0]);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } } */
/* { dg-final { scan-assembler-not "vcvtss2sd" } } */
/* { dg-final { scan-assembler-not "fld" } } */
/* { dg-final { scan-assembler-not "vcvtph2p\[sd\]" } } */
/* { dg-final { scan-assembler-not "vcvtps2pd" } } */
/* { dg-final { scan-assembler-not "extendhfxf" } } */
/* { dg-final { scan-assembler-not "\\\{1to8\\\}" } } */
/* { dg-final { scan-tree-dump-times "\.COPYSIGN" 12 "optimized" } } */
/* { dg-final { scan-assembler-times "vpternlog" 12 } } */
