/* { dg-additional-options "-fdump-tree-gimple" } */
/* complex reductions.  */

#define n 1000

int
main(void)
{
  int i;
  __complex__ double result, array[n];
  int lresult;

  /* '+' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (+:result)
  for (i = 0; i < n; i++)
    result += array[i];

  /* '*' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (*:result)
  for (i = 0; i < n; i++)
    result *= array[i];

  /* '&&' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (__real__(result) > __real__(array[i]));

  /* '||' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (||:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult || (__real__(result) > __real__(array[i]));

  return 0;
}

/* Check that default copy maps are generated for loop reductions.  */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:result \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:lresult \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
