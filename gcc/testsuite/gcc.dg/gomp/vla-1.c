// { dg-do compile }

void foo(int n)
{
  int A[n];

  #pragma omp parallel default(none)	// { dg-message "note: enclosing 'parallel'" }
    {
      A[0] = 0;				// { dg-error "'A' not specified" }
    }
}
