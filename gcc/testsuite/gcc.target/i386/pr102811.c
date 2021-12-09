/* { dg-do compile } */
/* { dg-options "-O2 -mf16c -mno-avx512fp16" } */
/* { dg-final { scan-assembler-times "vpxor\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtps2ph\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "__truncsfhf2\[ \\t\]"} } */
/* { dg-final { scan-assembler-not "__extendhfsf2\[ \\t\]"} } */
_Float16 test (_Float16 a, _Float16 b)
{
  return a + b;
}
