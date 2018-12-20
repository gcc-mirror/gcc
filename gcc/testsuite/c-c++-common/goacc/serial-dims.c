/* Invalid use of OpenACC parallelism dimensions clauses: num_gangs,
   num_workers, vector_length with the serial construct.  */

void f(void)
{
#pragma acc serial num_gangs (1) /* { dg-error "'num_gangs' is not valid for '#pragma acc serial'" } */
  ;
#pragma acc serial num_workers (1) /* { dg-error "'num_workers' is not valid for '#pragma acc serial'" } */
  ;
#pragma acc serial vector_length (1) /* { dg-error "'vector_length' is not valid for '#pragma acc serial'" } */
  ;
}
