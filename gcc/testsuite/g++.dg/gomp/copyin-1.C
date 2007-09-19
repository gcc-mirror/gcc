// { dg-do compile }
// { dg-require-effective-target tls_native }

int i, j;

#pragma omp threadprivate (i)

void bar(void);
void foo(void)
{
  int k;
  extern int l;
  extern int m;

#pragma omp threadprivate (m)

  #pragma omp parallel copyin(i)
    bar();
  #pragma omp parallel copyin(j)	// { dg-error "threadprivate" }
    bar();
  #pragma omp parallel copyin(k)	// { dg-error "threadprivate" }
    bar();
  #pragma omp parallel copyin(l)	// { dg-error "threadprivate" }
    bar();
  #pragma omp parallel copyin(m)
    bar();
}
