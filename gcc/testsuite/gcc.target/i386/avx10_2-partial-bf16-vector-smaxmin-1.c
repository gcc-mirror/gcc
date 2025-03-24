/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -Ofast" } */
/* { dg-final { scan-assembler-times "vmaxbf16" 2 } } */
/* { dg-final { scan-assembler-times "vminbf16" 2 } } */

void
maxpbf16_64 (__bf16* restrict dest, __bf16* restrict src1, __bf16* restrict src2)
{
  int i;
  for (i = 0; i < 4; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
maxpbf16_32 (__bf16* restrict dest, __bf16* restrict src1, __bf16* restrict src2)
{
  int i;
  for (i = 0; i < 2; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minpbf16_64 (__bf16* restrict dest, __bf16* restrict src1, __bf16* restrict src2)
{
  int i;
  for (i = 0; i < 4; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}

void
minpbf16_32 (__bf16* restrict dest, __bf16* restrict src1, __bf16* restrict src2)
{
  int i;
  for (i = 0; i < 2; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}
