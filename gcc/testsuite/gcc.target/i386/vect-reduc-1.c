/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mno-avx512f -fdump-tree-vect-details" } */

#define N 32
int foo (int *a, int n)
{
  int sum = 1;
  for (int i = 0; i < 8*N + 4; ++i)
    sum += a[i];
  return sum;
}

/* The reduction epilog should be vectorized and the accumulator
   re-used.  */
/* { dg-final { scan-tree-dump "LOOP EPILOGUE VECTORIZED" "vect" } } */
/* { dg-final { scan-assembler-times "psrl" 2 } } */
/* { dg-final { scan-assembler-times "padd" 5 } } */
