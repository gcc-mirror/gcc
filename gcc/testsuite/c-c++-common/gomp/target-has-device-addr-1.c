/* { dg-do compile } */

void
foo ()
{
  int * x;
  #pragma omp target is_device_ptr(x) has_device_addr(x) /*{ dg-error "'x' appears more than once in data clauses" } */
  ;
  #pragma omp target has_device_addr(x) is_device_ptr(x) /* { dg-error "'x' appears more than once in data clauses" } */
  ;

  int y = 42;
  #pragma omp target has_device_addr(y) has_device_addr(y) /* { dg-error "'y' appears more than once in data clauses" } */
  ;

  #pragma omp target private(y) has_device_addr(y) /*{ dg-error "'y' appears more than once in data clauses" } */
  ;
  #pragma omp target has_device_addr(y) private(y) /*{ dg-error "'y' appears more than once in data clauses" } */
  ;
  #pragma omp target firstprivate(y) has_device_addr(y) /*{ dg-error "'y' appears more than once in data clauses" } */
  ;

  #pragma omp target has_device_addr(y) map(y) /* { dg-error "'y' appears both in data and map clauses" } */
  ;
  #pragma omp target map(y) has_device_addr(y) /* { dg-error "'y' appears both in data and map clauses" } */
  ;

  int z[3] = { 2, 5, 7 };
  #pragma omp target data map(z[:3]) use_device_addr(z)
    #pragma omp target has_device_addr(z[1:])
    ;

  #pragma omp target data map(z[:3]) use_device_addr(z)
    #pragma omp target has_device_addr(z[1])
    ;

  #pragma omp target data map(z[:3]) use_device_addr(z)
    #pragma omp target has_device_addr(z[1:2])
    ;

  #pragma omp target data map(z[:3]) use_device_addr(z)
    #pragma omp target has_device_addr(z[:2])
    ;

  int w[3][4];
  #pragma omp target data map(w) use_device_addr(w)
    #pragma omp target has_device_addr(w[1][2])
    ;

  #pragma omp target data map(w) use_device_addr(w)
    #pragma omp target has_device_addr(w[:1][2:])
    ;

  int u[0];
  #pragma omp target data map(u) use_device_addr(u)
    #pragma omp target has_device_addr(u)
    ;

  struct S { int m; } s;
  s.m = 42;
  #pragma omp target data map (s) use_device_addr (s)
    #pragma omp target has_device_addr (s)
      ++s.m;

}
