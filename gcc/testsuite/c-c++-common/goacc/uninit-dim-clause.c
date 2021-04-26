/* { dg-additional-options "-Wuninitialized" } */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

void acc_parallel()
{
  int i, j, k;

  #pragma acc parallel num_gangs(i) /* { dg-warning "is used uninitialized" } */
  /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-1 } */
  ;

  #pragma acc parallel num_workers(j) /* { dg-warning "is used uninitialized" } */
  /* { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-1 } */
  ;

  #pragma acc parallel vector_length(k) /* { dg-warning "is used uninitialized" } */
  /* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 } */
  ;
}

void acc_kernels()
{
  int i, j, k;

  #pragma acc kernels num_gangs(i) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc kernels num_workers(j) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc kernels vector_length(k) /* { dg-warning "is used uninitialized" } */
  ;
}
