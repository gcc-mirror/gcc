/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O3" } */
/* { dg-final { scan-assembler-not "kmov" } } */
/* { dg-final { scan-assembler "kortest" } } */

int
foo (int *__restrict a, int* __restrict d, int b, int c, int n)
{
  for (int i = 0; i != 10000; i++)
    if (a[i] > b | d[i] > c)
      return 1;
  return 0;
}
