/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

/* Constant bound, vector addition.  */

#define N 1000

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];

void
f (void)
{
  int i;

    for (i = 0; i < N; ++i)
      c[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 1 "parloops2" } } */
