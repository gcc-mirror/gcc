#pragma omp assumes no_openmp_constructs

#pragma omp begin assumes no_openmp_constructs no_parallelism
void g(int x)
{
    if (x <= 0)
      return;
}
#pragma omp end assumes

void f()
{
  #pragma omp assume no_openmp no_openmp_something  // { dg-error "32: expected assumption clause" }
    ;

  #pragma omp assume no_openmp no_openmp_routines no_openmp_constructs no_openmp_constructs // { dg-error "72: too many 'no_openmp_constructs' clauses" }
    ;
}
