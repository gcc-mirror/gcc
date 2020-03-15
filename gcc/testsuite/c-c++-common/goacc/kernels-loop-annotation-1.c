/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that all loops in the nest are annotated.  */

void f (float a[16][16], float b[16][16], float c[16][16])
{
  int i, j, k;

#pragma acc kernels copyin(a[0:16][0:16], b[0:16][0:16]) copyout(c[0:16][0:16])
  {
    for (i = 0; i < 16; i++) {
      for (j = 0; j < 16; j++) {
	float t = 0;
	for (k = 0; k < 16; k++)
	  t += a[i][k] * b[k][j];
	c[i][j] = t;
      }
    }
  }

}

/* { dg-final { scan-tree-dump-times "acc loop auto" 3 "original" } } */
