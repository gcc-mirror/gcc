/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O3 -mno-avxvnni" } */
/* { dg-final { scan-assembler-times {(?n)vpmaddwd[^\n]*ymm} 4 } } */

int
foo (unsigned char* a, char* b, int n)
{
  int sum = 0;
  for (int i = 0; i != n; i++)
    sum += a[i] * b[i];
  return sum;
}
