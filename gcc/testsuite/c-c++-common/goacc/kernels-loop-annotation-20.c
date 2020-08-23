/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that calls to built-in functions don't inhibit kernels loop
   annotation.  */

void foo (int n, int *input, int *out1, int *out2)
{
#pragma acc kernels
  {
    int i;

    for (i = 0; i < n; i++)
      {
	out1[i] = __builtin_clz (input[i]);
	out2[i] = __builtin_popcount (input[i]);
      }
  }
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
