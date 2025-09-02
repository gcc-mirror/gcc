/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -Ofast -mgather" } */
/* { dg-final { scan-assembler-times {(?n)vfmadd[1-3]*ps[^\n]*ymm} 1 } } */

float
foo (float* a, int* b, float* c, int n)
{
  float sum = 0;
  for (int i = 0; i != n; i++)
    sum += a[b[i]] *c[i];
  return sum;
}

