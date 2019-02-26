void
f (int i, float j, int k)
{
#pragma acc parallel num_gangs (i) num_workers (i) vector_length (i)
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_gangs (j) /* { dg-error "'num_gangs' expression must be integral" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel num_workers (j) /* { dg-error "'num_workers' expression must be integral" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;

#pragma acc parallel vector_length (j) /* { dg-error "'vector_length' expression must be integral" } */
#pragma acc loop gang
  for (i = 0; i < 20; ++i)
    ;
}
