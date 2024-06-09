/* { dg-do compile { target riscv_v } }  */
/* { dg-additional-options "-std=c99" }  */

void
foo (int n, int **a)
{
  int b;
  for (b = 0; b < n; b++)
    for (long e = 8; e > 0; e--)
      a[b][e] = a[b][e] == 15;
}

/* { dg-final { scan-assembler "vmerge.vim" } }  */
