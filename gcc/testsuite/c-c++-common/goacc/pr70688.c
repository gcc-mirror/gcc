/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

const int n = 100;

int
private_reduction ()
{
  int i, r;

  #pragma acc parallel
  #pragma acc loop private (r) reduction (+:r)
  for (i = 0; i < 100; i++)
    r += 10;

  return r;
}

int
parallel_reduction ()
{
  int sum = 0;
  int dummy = 0;

#pragma acc data copy (dummy)
  {
#pragma acc parallel num_gangs (10) copy (sum) reduction (+:sum)
    /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 } */
    {
      int v = 5;
      sum += 10 + v;
    }
  }

  return sum;
}

int
main ()
{
  int i, s = 0;

#pragma acc parallel num_gangs (10) copy (s) reduction (+:s)
  /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 } */
  for (i = 0; i < n; i++)
    s += i+1;

#pragma acc parallel num_gangs (10) reduction (+:s) copy (s)
  /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 } */
  for (i = 0; i < n; i++)
    s += i+1;

  return 0;
}
