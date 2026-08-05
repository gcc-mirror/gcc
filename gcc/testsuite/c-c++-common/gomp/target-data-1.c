/* { dg-do compile } */

void
foo (void)
{
  int a[4] = { 1, 2, 3, 4 };
  int *p = &a[0];
  int x = 5;
  #pragma omp target data map(to:p[:4])
  #pragma omp target data use_device_ptr(p)
  #pragma omp target is_device_ptr(p)
  {
    p[0]++;
  }
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_addr(a)
  #pragma omp target is_device_ptr(a)
  {
    p[0]++;
  }
  #pragma omp target data map(to:x)
  #pragma omp target data use_device_addr(x)
  {
    int *q = &x;
    #pragma omp target is_device_ptr(q)
    {
      q[0]++;
    }
  }
  #pragma omp target data		/* { dg-error "must contain at least one" } */
  a[0]++;
  #pragma omp target data map(to:p)
  #pragma omp target data use_device_ptr(p) use_device_ptr(p) /* { dg-error "appears more than once in data clauses" } */
  a[0]++;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_addr(a) use_device_addr(a) /* { dg-error "appears more than once in data clauses" } */
  a[0]++;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(a)	/* { dg-error "'use_device_ptr' variable is not a pointer" "" { target c } } */
						/* { dg-error "'use_device_ptr' variable is neither a pointer nor reference to pointer" "" { target c++ } .-1 } */
  a[0]++;					/* { dg-error "must contain at least one" "" { target *-*-* } .-2 } */
}
