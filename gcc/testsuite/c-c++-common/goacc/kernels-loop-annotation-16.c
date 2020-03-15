/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a modification of the loop variable in the
   body cannot be annotated.  */

float f (float *a, float *b, int n)
{
  float t = 0;

#pragma acc kernels
  {
    for (int i = 0; i < n; i++)	/* { dg-warning "loop cannot be annotated" } */
      {
	if (a[i] < 0 || b[i] < 0)
	  i = n;
	else
	  t += a[i] * b[i];
      }
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } } */
