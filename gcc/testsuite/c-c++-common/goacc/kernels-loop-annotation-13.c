/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that in a situation with nested loops, a problem that prevents
   annotation of the outer loop only still allows the inner loop to be
   annotated.  */

float f (float *a, float *b, int n)
{
  float t = 0;

#pragma acc kernels
  {
    for (int i = 0; i < n; i++)	  /* { dg-warning "loop cannot be annotated" } */
      {
	if (a[i] < 0)
	  n = i;
	for (int j = 0; j <= i; j++)
	  t += a[i] * b[j];
      }
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
