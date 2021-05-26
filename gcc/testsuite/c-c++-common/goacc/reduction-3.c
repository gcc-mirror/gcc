/* { dg-additional-options "-fdump-tree-gimple" } */
/* double reductions.  */

#define n 1000

int
main(void)
{
  int i;
  double result, array[n];
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

  /* 'max' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (max:result)
  for (i = 0; i < n; i++)
    result = result > array[i] ? result : array[i];

  /* 'min' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (min:result)
  for (i = 0; i < n; i++)
    result = result < array[i] ? result : array[i];

  /* '&&' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (result > array[i]);

  /* '||' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (||:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult || (result > array[i]);

  return 0;
}

/* Check that default copy maps are generated for loop reductions.  */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:result \\\[len: \[0-9\]+\\\]\\)" 4 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:lresult \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
