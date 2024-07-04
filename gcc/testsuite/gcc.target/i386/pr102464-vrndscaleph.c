/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl -mprefer-vector-width=512" } */
#ifndef __NO_MATH_INLINES
#define __NO_MATH_INLINES
#endif
#include<math.h>
void
foo (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 8; i++)
      a[i] = floor (b[i]);
}

void
foo1 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 8; i++)
      a[i] = ceil (b[i]);
}

void
foo2 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 8; i++)
      a[i] = trunc (b[i]);
}

void
foo3 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 8; i++)
      a[i] = nearbyint (b[i]);
}

void
foo4 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 8; i++)
      a[i] = rint (b[i]);
}

void
foo5 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 16; i++)
      a[i] = floor (b[i]);
}

void
foo6 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 16; i++)
      a[i] = ceil (b[i]);
}

void
foo7 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 16; i++)
      a[i] = trunc (b[i]);
}

void
foo8 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 16; i++)
      a[i] = nearbyint (b[i]);
}

void
foo9 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 16; i++)
      a[i] = rint (b[i]);
}

void
foo10 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 32; i++)
      a[i] = floor (b[i]);
}

void
foo11 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 32; i++)
      a[i] = ceil (b[i]);
}

void
foo12 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 32; i++)
      a[i] = trunc (b[i]);
}

void
foo13 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 32; i++)
      a[i] = nearbyint (b[i]);
}

void
foo14 (_Float16* __restrict a, _Float16* b)
{
    for (int i = 0; i != 32; i++)
      a[i] = rint (b[i]);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } } */
/* { dg-final { scan-assembler-not "vcvtph2p\[sd\]" } } */
/* { dg-final { scan-assembler-not "extendhfxf" } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[^\n\r\]*xmm\[0-9\]" 5 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[^\n\r\]*ymm\[0-9\]" 5 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[^\n\r\]*zmm\[0-9\]" 5 } } */
