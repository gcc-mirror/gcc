/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test that "acc kernels loop" directive causes annotation of the entire
   loop nest in the presence of a collapse clause.  */

void f (float *a, float *b)
{
#pragma acc kernels loop collapse(2)
  for (int k = 0; k < 20; k++)
    for (int l = 0; l < 20; l++)
      for (int m = 0; m < 20; m++)
	b[m] = a[m];
}

/* { dg-final { scan-tree-dump-times "acc loop collapse.2." 1 "original" } } */
/* { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } } */
