/* Ensure that the -fverbose-asm leads to source code information in the generated asm.  */
/* { dg-options "-fverbose-asm" } */

int test (int n)
{
  int i;
  int total = 0;

  for (i = 0; i < n; i++)
    total += i * i;

  return total;
}

/* { dg-final { scan-assembler "total = 0" } } */
