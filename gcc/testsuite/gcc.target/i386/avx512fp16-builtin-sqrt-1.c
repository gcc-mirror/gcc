/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mprefer-vector-width=512" } */

_Float16
f1 (_Float16 x)
{
  return __builtin_sqrtf16 (x);
}

void
f2 (_Float16* __restrict psrc, _Float16* __restrict pdst)
{
  for (int i = 0; i != 32; i++)
    pdst[i] = __builtin_sqrtf16 (psrc[i]);
}

/* { dg-final { scan-assembler-times "vsqrtsh\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vsqrtph\[^\n\r\]*zmm\[0-9\]" 1 } } */
