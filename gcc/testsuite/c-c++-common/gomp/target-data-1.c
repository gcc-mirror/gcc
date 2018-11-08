/* { dg-do compile } */

void
foo (void)
{
  int a[4] = { 1, 2, 3, 4 };
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(a)
  #pragma omp target is_device_ptr(a)
  {
    a[0]++;
  }
  #pragma omp target data		/* { dg-error "must contain at least one" } */
  a[0]++;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(a) use_device_ptr(a) /* { dg-error "appears more than once in data clauses" } */
  a[0]++;
}
