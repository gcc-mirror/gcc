/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -mprefer-vector-width=128 -O3" } */
/* { dg-final { scan-assembler "kortest" } } */
/* { dg-final { scan-assembler-not "kmov" } } */

int
foo (int *__restrict a)
{
  for (int i = 0; i != 100; i++)
    if (a[i] == 0)
      return 1;
  return 0;
}
