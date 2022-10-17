/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl" } */

void
f1 (_Float16* __restrict psrc, _Float16* __restrict pdst)
{
  for (int i = 0; i != 8; i++)
    pdst[i] = __builtin_sqrtf16 (psrc[i]);
}

void
f2 (_Float16* __restrict psrc, _Float16* __restrict pdst)
{
  for (int i = 0; i != 16; i++)
    pdst[i] = __builtin_sqrtf16 (psrc[i]);
}

/* { dg-final { scan-assembler-times "vsqrtph\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vsqrtph\[^\n\r\]*ymm\[0-9\]" 1 } } */
