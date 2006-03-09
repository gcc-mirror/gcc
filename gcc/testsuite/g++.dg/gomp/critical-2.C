// { dg-do compile }

void f1(void)
{
  #pragma omp critical a	// { dg-error "expected" }
    ;
  #pragma omp critical (	// { dg-error "expected identifier" }
    ;
  #pragma omp critical (a	// { dg-error "expected .\\)." }
    ;
  #pragma omp critical (a b)	// { dg-error "expected .\\)." }
}				// { dg-error "" }
