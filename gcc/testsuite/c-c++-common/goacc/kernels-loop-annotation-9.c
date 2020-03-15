/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a kernels loop with a return in the body triggers a hard
   error.  */

#define n 16

float f (float *a, float *b)
{
  float t = 0;
  int i;

#pragma acc kernels
  {
    for (i = 0; i < n; i++)
      {
	if (a[i] < 0 || b[i] < 0)
	  return 0.0;	/* { dg-error "invalid branch" } */
	t += a[i] * b[i];
      }
  }
  return t;
}
