/* Verify that the error message for gang reduction on orphaned OpenACC loops
   is not reported for non-orphaned loops. */

/* { dg-additional-options "-Wopenacc-parallelism" } */

int
kernels (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc kernels
  {
#pragma acc loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;
  }
  return s1 + s2;
}

int
parallel (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc parallel
  {
#pragma acc loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;
  }
  return s1 + s2;
}

int
serial (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc serial /* { dg-warning "region contains gang partitioned code but is not gang partitioned" } */
  {
#pragma acc loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;
  }
  return s1 + s2;
}

int
serial_combined (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc serial loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  /* { dg-warning "region contains gang partitioned code but is not gang partitioned" "" { target *-*-* } .-1 } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc serial loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  /* { dg-warning "region contains gang partitioned code but is not gang partitioned" "" { target *-*-* } .-1 } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;

  return s1 + s2;
}

int
parallel_combined (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc parallel loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc parallel loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;

  return s1 + s2;
}

int
kernels_combined (int n)
{
  int i, s1 = 0, s2 = 0;
#pragma acc kernels loop gang reduction(+:s1) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s1 = s1 + 2;

#pragma acc kernels loop gang reduction(+:s2) /* { dg-bogus "gang reduction on an orphan loop" } */
  for (i = 0; i < n; i++)
    s2 = s2 + 2;

  return s1 + s2;
}
