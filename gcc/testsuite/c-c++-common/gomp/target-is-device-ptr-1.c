/* { dg-do compile } */

void
foo ()
{
  int *x;

  #pragma omp target is_device_ptr(x) is_device_ptr(x) /* { dg-error "'x' appears more than once in data clauses" } */
  ;

  #pragma omp target private(x) is_device_ptr(x) /*{ dg-error "'x' appears more than once in data clauses" } */
  ;
  #pragma omp target is_device_ptr(x) private(x) /*{ dg-error "'x' appears more than once in data clauses" } */
  ;
  #pragma omp target firstprivate(x) is_device_ptr(x) /*{ dg-error "'x' appears more than once in data clauses" } */
  ;

  #pragma omp target is_device_ptr(x) map(x) /* { dg-error "'x' appears both in data and map clauses" } */
  ;
  #pragma omp target map(x) is_device_ptr(x) /* { dg-error "'x' appears both in data and map clauses" } */
  ;
}
