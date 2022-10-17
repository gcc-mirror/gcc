/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ffast-math -ftree-vectorize" } */

#include<math.h>
void foo1 (_Float16* __restrict a, _Float16* b)
{
  for (int i = 0; i != 8; i++)
    a[i] =  sqrtf (b[i]);
}

void foo2 (_Float16* __restrict a, _Float16* b)
{
  for (int i = 0; i != 8; i++)
    a[i] =  sqrt (b[i]);
}

void foo3 (_Float16* __restrict a, _Float16* b)
{
  for (int i = 0; i != 8; i++)
    a[i] =  sqrtl (b[i]);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } } */
/* { dg-final { scan-assembler-not "vcvtph2p\[sd\]" } } */
/* { dg-final { scan-assembler-not "extendhfxf" } } */
/* { dg-final { scan-assembler-times "vsqrtph\[^\n\r\]*xmm\[0-9\]" 3 } } */
