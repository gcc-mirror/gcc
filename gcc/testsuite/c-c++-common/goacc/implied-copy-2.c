/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test that reduction on compute construct implies a copy of the reduction
  variable .  */

#define n 1000

#if __cplusplus
  typedef bool BOOL;
#else
  typedef _Bool BOOL;
#endif

int
main(void)
{
  int i;
  int sum = 0;
  int prod = 1;
  int result = 0;
  int tmp = 1;
  int array[n];

  double sumd = 0.0;
  double arrayd[n];

  float sumf = 0.0;
  float arrayf[n];

  char sumc;
  char arrayc[n];

  BOOL lres;

#pragma acc parallel reduction(+:sum, sumf, sumd, sumc) reduction(*:prod)
  for (i = 0; i < n; i++)
    {
      sum += array[i];
      sumf += arrayf[i];
      sumd += arrayd[i];
      sumc += arrayc[i];
      prod *= array[i];
    }

#pragma acc parallel reduction (max:result)
  for (i = 0; i < n; i++)
    result = result > array[i] ? result : array[i];

#pragma acc parallel reduction (min:result)
  for (i = 0; i < n; i++)
    result = result < array[i] ? result : array[i];

#pragma acc parallel reduction (&:result)
  for (i = 0; i < n; i++)
    result &= array[i];

#pragma acc parallel reduction (|:result)
  for (i = 0; i < n; i++)
    result |= array[i];

#pragma acc parallel reduction (^:result)
  for (i = 0; i < n; i++)
    result ^= array[i];

#pragma acc parallel reduction (&&:lres) copy(tmp)
  for (i = 0; i < n; i++)
    lres = lres && (tmp > array[i]);

#pragma acc parallel reduction (||:lres) copy(tmp)
  for (i = 0; i < n; i++)
    lres = lres || (tmp > array[i]);

  /* Same checks on serial construct.  */
#pragma acc serial reduction(+:sum, sumf, sumd, sumc) reduction(*:prod)
  for (i = 0; i < n; i++)
    {
      sum += array[i];
      sumf += arrayf[i];
      sumd += arrayd[i];
      sumc += arrayc[i];
      prod *= array[i];
    }

#pragma acc serial reduction (max:result)
  for (i = 0; i < n; i++)
    result = result > array[i] ? result : array[i];

#pragma acc serial reduction (min:result)
  for (i = 0; i < n; i++)
    result = result < array[i] ? result : array[i];

#pragma acc serial reduction (&:result)
  for (i = 0; i < n; i++)
    result &= array[i];

#pragma acc serial reduction (|:result)
  for (i = 0; i < n; i++)
    result |= array[i];

#pragma acc serial reduction (^:result)
  for (i = 0; i < n; i++)
    result ^= array[i];

#pragma acc serial reduction (&&:lres) copy(tmp)
  for (i = 0; i < n; i++)
    lres = lres && (tmp > array[i]);

#pragma acc serial reduction (||:lres) copy(tmp)
  for (i = 0; i < n; i++)
    lres = lres || (tmp > array[i]);

  return 0;
}

/* { dg-final { scan-tree-dump-times "map\\(tofrom:sum \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:sumf \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:sumd \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:sumc \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:lres \\\[len: \[0-9\]+\\\]\\)" 4 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:result \\\[len: \[0-9\]+\\\]\\)" 10 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:prod \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
