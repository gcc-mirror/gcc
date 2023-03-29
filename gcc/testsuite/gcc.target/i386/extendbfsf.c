/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times "pslld" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16" 1 } } */

float
extendsfbf (__bf16 a)
{
  return a;
}

__bf16
truncsfbf (float a)
{
  return a;
}
