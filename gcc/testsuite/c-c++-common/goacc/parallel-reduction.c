/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

int
main ()
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
