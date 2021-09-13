/* Valid use of OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

void f(int i)
{
#pragma acc kernels num_gangs(i) num_workers(i) vector_length(i)
  ;

#pragma acc parallel num_gangs(i) num_workers(i) vector_length(i)
  /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO runtime" { xfail *-*-* } .-1 }
     { dg-bogus "warning: region is worker partitioned but does not contain worker partitioned code" "TODO runtime" { xfail *-*-* } .-2 }
     { dg-bogus "warning: region is vector partitioned but does not contain vector partitioned code" "TODO runtime" { xfail *-*-* } .-3 }
     TODO 'region is [...] partitioned' isn't correct for 'i == 1'.  */
  ;
}
