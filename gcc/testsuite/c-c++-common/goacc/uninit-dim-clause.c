/* { dg-additional-options "-Wuninitialized" } */

void acc_parallel()
{
  int i, j, k;

  #pragma acc parallel num_gangs(i) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc parallel num_workers(j) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc parallel vector_length(k) /* { dg-warning "is used uninitialized in this function" } */
  ;
}

void acc_kernels()
{
  int i, j, k;

  #pragma acc kernels num_gangs(i) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc kernels num_workers(j) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc kernels vector_length(k) /* { dg-warning "is used uninitialized in this function" } */
  ;
}
