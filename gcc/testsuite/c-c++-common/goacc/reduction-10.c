/* { dg-additional-options "-fdump-tree-gimple" } */

#define n 1000

int
main(void)
{
  int i, j;
  int result, array[n];

#pragma acc parallel loop reduction (+:result)
  for (i = 0; i < n; i++)
    result ++;

#pragma acc parallel
#pragma acc loop reduction (+:result)
  for (i = 0; i < n; i++)
    result ++;

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < n; i++)
    {
      result = i;

#pragma acc loop reduction(+:result)
      for (j = 0; j < n; j++)
	result ++;

      array[i] = result;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < n; i++)
    {
      result = i;

#pragma acc loop worker vector reduction(+:result)
      for (j = 0; j < n; j++)
	result ++;

      array[i] = result;
    }

#pragma acc parallel
#pragma acc loop // { dg-warning "insufficient partitioning" }
  for (i = 0; i < n; i++)
    {
      result = i;

#pragma acc loop gang reduction(+:result)
      for (j = 0; j < n; j++)
	result ++;

      array[i] = result;
    }

#pragma acc parallel copy(result)
#pragma acc loop // { dg-warning "insufficient partitioning" }
  for (i = 0; i < n; i++)
    {
      result = i;

#pragma acc loop gang reduction(+:result)
      for (j = 0; j < n; j++)
	result ++;

      array[i] = result;
    }
  
#pragma acc kernels
#pragma acc loop
  for (i = 0; i < n; i++)
    {
      result = i;

#pragma acc loop reduction(+:result)
      for (j = 0; j < n; j++)
	result ++;

      array[i] = result;
    }

  return 0;
}

/* Check that default copy maps are generated for loop reductions.  */
/* { dg-final { scan-tree-dump-times "reduction..:result. map.tofrom:result .len: 4.." 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "oacc_parallel map.tofrom:result .len: 4.." 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map.tofrom:array .len: 4000.. firstprivate.result." 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map.tofrom:result .len: 4.. map.tofrom:array .len: 4000.." 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map.tofrom:array .len: 4000.. map.force_tofrom:result .len: 4.." 1 "gimple" } } */
