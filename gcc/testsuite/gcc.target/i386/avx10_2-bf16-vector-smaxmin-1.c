/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-256 -Ofast" } */
/* { dg-final { scan-assembler-times "vmaxbf16" 2 } } */
/* { dg-final { scan-assembler-times "vminbf16" 2 } } */

void
maxbf16_256 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minbf16_256 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}

void
maxbf16_128 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minbf16_128 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}
