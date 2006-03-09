// { dg-do compile }

struct T
{
  int n;

  void test();
};

void T::test()
{
  #pragma omp parallel private(n)	// { dg-error "T::n" }
    n = 1;

  #pragma omp parallel shared(n)	// { dg-error "T::n" }
    n = 1;

  #pragma omp parallel firstprivate(n)	// { dg-error "T::n" }
    n = 1;

  #pragma omp sections lastprivate(n)	// { dg-error "T::n" }
    { n = 1; }

  #pragma omp parallel reduction(+:n)	// { dg-error "T::n" }
    n = 1;

  #pragma omp single copyprivate(n)	// { dg-error "T::n" }
    n = 1;

  #pragma omp parallel copyin(n)	// { dg-error "T::n" }
    n = 1;
}
