/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test for rejecting annotation on loops that have various subexpressions
   in the loop end test that are not loop-invariant.  */

extern int g (int);
extern int x;
extern int gg (int, int) __attribute__ ((const));

void f (float *a, float *b, int n)
{

  int j;
#pragma acc kernels
  {
    /* Non-constant function call.  */
    for (int i = 0; i < g(n); i++)	/* { dg-warning "loop cannot be annotated" } */
      a[i] = b[i];

    /* Global variable.  */
    for (int i = x; i < n + x; i++)	/* { dg-warning "loop cannot be annotated" } */
      a[i] = b[i];

    /* Explicit reference to the loop variable.  */
    for (int i = 0; i < gg (i, n); i++)	/* { dg-warning "loop cannot be annotated" } */
      a[i] = b[i];

    /* Reference to a variable that is modified in the body of the loop.  */
    j = 0;
    for (int i = 0; i < gg (j, n); i++)	/* { dg-warning "loop cannot be annotated" } */
      {
	a[i] = b[i];
	j = i;
      }

  }
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } } */
