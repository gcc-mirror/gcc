/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a continue statement in the body can be annotated.  */

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
	  continue;
	t += a[i] * b[i];
      }
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
