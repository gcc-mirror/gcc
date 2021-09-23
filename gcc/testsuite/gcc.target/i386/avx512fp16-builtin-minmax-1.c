/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mprefer-vector-width=512" } */

_Float16
minf1 (_Float16 a, _Float16 b)
{
  return __builtin_fminf16 (a, b);
}

void
minf2 (_Float16* __restrict psrc1, _Float16* __restrict psrc2,
       _Float16* __restrict pdst)
{
  for (int i = 0; i != 32; i++)
    pdst[i] = __builtin_fminf16 (psrc1[i], psrc2[i]);
}

_Float16
maxf1 (_Float16 a, _Float16 b)
{
  return __builtin_fmaxf16 (a, b);
}

void
maxf2 (_Float16* __restrict psrc1, _Float16* __restrict psrc2,
       _Float16* __restrict pdst)
{
  for (int i = 0; i != 32; i++)
    pdst[i] = __builtin_fmaxf16 (psrc1[i], psrc2[i]);
}

/* { dg-final { scan-assembler-times "vmaxsh\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vmaxph\[^\n\r\]*zmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vminsh\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vminph\[^\n\r\]*zmm\[0-9\]" 1 } } */
