/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that a loop with a random goto in the body can't be annotated.  */

#define n 16

float f (float *a, float *b)
{
  float t = 0;
  int i;

#pragma acc kernels
  {
    for (i = 0; i < n; i++)	/* { dg-warning "loop cannot be annotated" } */
      {
	if (a[i] < 0)
	  {
	    t = 0;
	    goto bad;
	  }
	t += a[i] * b[i];
      }
  bad:
    ;
  }
  return t;
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } } */
