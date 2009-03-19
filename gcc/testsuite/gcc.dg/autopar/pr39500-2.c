/* pr39500: autopar fails to parallel */
/* origin: nemokingdom@gmail.com(LiFeng) */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details" } */

int main (void)
{
  int i;
  int x[1000];

  for (i = 0; i < 101; i++)
    x[i] = x[i+100];

  return x[12];
}

/* This loop cannot be parallelized due to a dependence.  */

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 0 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
