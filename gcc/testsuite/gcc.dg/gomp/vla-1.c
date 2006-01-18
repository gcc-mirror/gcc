// { dg-do compile }

void foo(int n)
{
  int A[n];

  #pragma omp parallel default(none)	// { dg-error "enclosing" }
    {
      A[0] = 0;				// { dg-error "'A' not specified" }
    }
}
