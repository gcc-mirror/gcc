/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that an explicit annotation on an inner loop suppresses annotation
   of outer loops, and produces a diagnostic.  */

void f (float *a, float *b)
{
  float t = 0;

#pragma acc kernels
  {
    for (int l = 0; l < 20; l++)	/* { dg-warning "loop cannot be annotated" } */
#pragma acc loop seq
      for (int m = 0; m < 20; m++)
        b[m] = a[m];
  }
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } } */
