/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a switch and break in the body can be annotated.  */

#define n 16

float f (float *a, float *b, int state)
{
  float t = 0;
  int i;

#pragma acc kernels
  {
    for (i = 0; i < n; i++)
      switch (state)
	{
	case 0:
	default:
	  t += a[i] * b[i];
	  break;

	case 1:
	  if (a[i] > 0 && b[i] > 0)
	    t += a[i] * b[i];
	  break;
	}
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
