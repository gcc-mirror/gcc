/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_unroll" } */

void __attribute__ ((noinline)) foo(int n, int *arr)
{
  int i;
  for (i = 0; i < n; i++)
    arr[i] = arr[i] - 10;
}
/* { dg-final { scan-rtl-dump-times "Unrolled loop 1 times" 1 "loop2_unroll" } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 3 } } */
/* { dg-final { scan-assembler-times {\mstw\M} 3 } } */

