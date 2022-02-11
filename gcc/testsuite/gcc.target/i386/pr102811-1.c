/* { dg-do compile } */
/* { dg-options "-O2 -mf16c -mno-avx512fp16" } */
/* { dg-final { scan-assembler-times "vpxor" 1 } } */
/* { dg-final { scan-assembler-times "vpblendw" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpinsrw" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-not "vpextrw"} } */
/* { dg-final { scan-assembler-not "vpbroadcastw"} } */
_Float16 test (_Float16 a, _Float16 b)
{
  return a + b;
}
