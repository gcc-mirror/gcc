/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -mprefer-vector-width=512 -Ofast" } */
/* /* { dg-final { scan-assembler-times "vmaxpbf16" 1 } } */
/* /* { dg-final { scan-assembler-times "vminpbf16" 1 } } */

void
maxpbf16_512 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 32; i++)
    dest[i] = src1[i] > src2[i] ? src1[i] : src2[i];
}

void
minpbf16_512 (__bf16* dest, __bf16* src1, __bf16* src2)
{
  int i;
  for (i = 0; i < 32; i++)
    dest[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}
