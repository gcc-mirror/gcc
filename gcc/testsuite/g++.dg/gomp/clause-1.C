// { dg-do compile }

struct T
{
  int n;

  void test();
};

void T::test()
{
  #pragma omp parallel private(n)
    n = 1;

  #pragma omp parallel shared(n)
  #pragma omp single
    n = 1;

  #pragma omp parallel firstprivate(n)
    n = 1;

  #pragma omp sections lastprivate(n)
    { n = 1; }

  #pragma omp parallel reduction(+:n)
    n = 1;

  #pragma omp single copyprivate(n)
    n = 1;

  #pragma omp parallel copyin(n)	// { dg-error "T::n" }
    n = 1;
}
