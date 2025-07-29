/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -Ofast" } */
/* { dg-final { scan-assembler-times {(?n)vfnmadd[1-3]*ps[^\n]*ymm} 4 } } */

float
foo (float* a, float* b, int n)
{
  float sum = 0;
  for (int i = 0; i != n; i++)
    sum -= a[i] * b[i];
  return sum;
}
