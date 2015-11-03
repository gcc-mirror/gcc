// PR c++/67522
// { dg-do compile }
// { dg-options "-fopenmp" }

struct S;

template <int N>
void
foo (void)
{
  #pragma omp simd linear (S)			// { dg-error "is not a variable in clause" }
  for (int i = 0; i < 16; i++)
    ;

  #pragma omp target map (S[0:10])		// { dg-error "is not a variable in" }
  ;

  #pragma omp task depend (inout: S[0:10])	// { dg-error "is not a variable in" }
  ;

  #pragma omp for reduction (+:S[0:10])		// { dg-error "is not a variable in" }
  for (int i = 0; i < 16; i++)
    ;
}

void
bar ()
{
  foo <0> ();
}
