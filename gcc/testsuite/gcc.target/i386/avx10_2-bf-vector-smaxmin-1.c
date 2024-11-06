/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -Ofast" } */
/* { dg-final { scan-assembler-times "vmaxpbf16" 2 } } */
/* { dg-final { scan-assembler-times "vminpbf16" 2 } } */

void
maxpbf16_256 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minpbf16_256 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}

void
maxpbf16_128 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minpbf16_128 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 16; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}
