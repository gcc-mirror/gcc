void
f (int i, int j, int k)
{
#pragma acc kernels num_gangs (10) /* { dg-error "'num_gangs' is not valid" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels num_workers (10) /* { dg-error "'num_workers' is not valid" } */
#pragma acc loop worker
  for (i = 0; i < 20; ++i)
    ;

#pragma acc kernels vector_length (10) /* { dg-error "'vector_length' is not valid" } */
#pragma acc loop vector
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_gangs (10) num_workers (20) vector_length (32)
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_gangs (i) num_workers (j) vector_length (k)
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_gangs (10, i) /* { dg-error "expected '\\)' before ',' token" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_workers (10, i) /* { dg-error "expected '\\)' before ',' token" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel vector_length (10, i) /* { dg-error "expected '\\)' before ',' token" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;
}
