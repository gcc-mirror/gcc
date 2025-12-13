// tests for deprecation of 'to' clause with 'declare target' and the '-'
// operator when used in reductions.

int x = 24;

#pragma omp declare target to(x)	// { dg-warning "'to' clause with 'declare target' deprecated since OpenMP 5.2, use 'enter' \\\[-Wdeprecated-openmp\\\]" }

int foo(int x)
{
  return x + 1;
}
#pragma omp declare target to(foo)	// { dg-warning "'to' clause with 'declare target' deprecated since OpenMP 5.2, use 'enter' \\\[-Wdeprecated-openmp\\\]" }

int
main()
{
  int y;
  int result = 0;

  #pragma omp target map(from:y)
  y = foo(x);

  #pragma omp parallel for reduction(-: result) // { dg-warning "'-' operator for reductions deprecated in OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" }
  for (int i = 0; i < 8; ++i)
    result -= i;

  return 0;
}
