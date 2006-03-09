// { dg-do compile }

void f1(void)
{
  #pragma omp flush a		// { dg-error "expected" }
  #pragma omp flush (		// { dg-error "expected" }
  #pragma omp flush (b		// { dg-error "declared|expected" }
  #pragma omp flush (c d)	// { dg-error "declared|expected" }
  #pragma omp flush (e)		// { dg-error "declared" }
}
