/* { dg-do compile } */
/* { dg-options "-mavxvnni -O3" } */
/* { dg-final { scan-assembler-times {(?n)vpdpbusd[^\n]*ymm} 4 } } */

int
foo (unsigned char* a, char* b, int n)
{
  int sum = 0;
  for (int i = 0; i != n; i++)
    sum += a[i] * b[i];
  return sum;
}
