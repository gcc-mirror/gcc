/* { dg-do compile } */

void
foo (int n)
{
  /* Test to ensure that 'device_num' is parsed correctly in device clauses. */

  #pragma omp target device (1)
  ;

  #pragma omp target device (n)
  ;

  #pragma omp target device (n + 1)
  ;

  #pragma omp target device (device_num : 1)
  ;

  #pragma omp target device (device_num : n)
  ;

  #pragma omp target device (device_num : n + 1)
  ;

  #pragma omp target device (invalid : 1) /* { dg-error "expected 'ancestor' or 'device_num'" "" { target *-*-* } } */
  /* { dg-error "expected '\\)' before 'invalid'" "" { target c } .-1 } */
  ;

  #pragma omp target device (device_num : n, n) /* { dg-error "expected '\\)' before ','" } */
  ;
}
