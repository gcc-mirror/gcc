/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

void foo (int * __restrict__ a,
	  int * __restrict__ b,
	  int * __restrict__ c)
{
  int i;

  for (i=1; i < 10; i++)
    {
      a[i] += c[i];
      b[i] = a[i - 1] + 1;
    }

  /* This loop is not distributed because the cost of spliting it:

     |  for (i=1; i < N; i++)
     |    a[i] += c[i];
     |
     |  for (i=1; i < N; i++)
     |    b[i] = a[i - 1] + 1;

     is higher due to data in array A that is written and then read in
     another task.  The cost model should forbid the transformation in
     this case.
  */
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
