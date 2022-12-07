/* { dg-do compile } */
/* { dg-options "-fexcess-precision=16 -O -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "pslld" 4 } } */

char
foo (__bf16 a, __bf16 b)
{
  return a > b;
}

float
foo1 (__bf16 a, __bf16 b, float c, float d)
{
  return a > b ? c : d;
}
