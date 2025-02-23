/* { dg-additional-options "-fdump-tree-gimple" } */
/* complex array reductions.  */

#define n 1000

int
main(void)
{
  int i, j;
  __complex__ double result[n], array[n];
  int lresult[n];

  /* '+' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (+:result)
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      result[j] += array[i];

  /* '*' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (*:result)
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      result[j] *= array[i];

  /* '&&' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (&&:lresult)
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      lresult[j] = lresult[j] && (__real__(result[j]) > __real__(array[i]));

  /* '||' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (||:lresult)
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      lresult[j] = lresult[j] || (__real__(result[j]) > __real__(array[i]));

  return 0;
}

/* Check that default copy maps are generated for loop reductions.  */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:result \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\)" 4 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:lresult \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\)" 2 "gimple" } } */
