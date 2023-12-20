/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test for implied copy of reduction variable on combined construct.  */
void test1 (void)
{
  int i, sum = 0, prod = 1, a[100];

  #pragma acc kernels loop reduction(+:sum) reduction(*:prod)
  for (int i = 0; i < 10; ++i)
  {
    sum += a[i];
    prod *= a[i];
  }

  #pragma acc parallel loop reduction(+:sum) reduction(*:prod)
  for (int i = 0; i < 10; ++i)
  {
    sum += a[i];
    prod *= a[i];
  }

  #pragma acc serial loop reduction(+:sum) reduction(*:prod)
  for (int i = 0; i < 10; ++i)
  {
    sum += a[i];
    prod *= a[i];
  }
}

/* { dg-final { scan-tree-dump-times "map\\(force_tofrom:sum \\\[len: \[0-9\]+\\\]\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(force_tofrom:prod \\\[len: \[0-9\]+\\\]\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:sum \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:prod \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
