/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a random label in the body triggers a warning.  */

#define n 16

float f (float *a, float *b)
{
  float t = 0;
  int i = n - 1;

#pragma acc kernels
  {
    goto spaghetti;
    for (i = 0; i < n; i++)	/* { dg-warning "loop cannot be annotated" } */
      {
      spaghetti:
	t += a[i] * b[i];
      }
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } } */
