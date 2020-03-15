/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a variable bound can be annotated.  */

float f (float *a, float *b, int n)
{
  float t = 0;
  int i;

#pragma acc kernels
  {
    for (i = 0; i < n; i++)
      t += a[i] * b[i];
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
