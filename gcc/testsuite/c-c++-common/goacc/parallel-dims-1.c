/* Valid use of OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

void f(int i)
{
#pragma acc kernels \
  num_gangs(i) num_workers(i) vector_length(i)
  ;

#pragma acc parallel /* { dg-bogus "region is (gang|worker|vector) partitioned" "" { xfail *-*-* } } */ \
  num_gangs(i) num_workers(i) vector_length(i)
  ;
}
