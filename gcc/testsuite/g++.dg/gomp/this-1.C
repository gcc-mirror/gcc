// { dg-do compile }
// { dg-options "-fopenmp" }

struct S
{
  #pragma omp declare simd linear(this)	// { dg-error "invalid use of .this" }
  static void foo ();
  void bar ();
};

void
S::bar ()
{
  #pragma omp parallel firstprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp parallel for lastprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  for (int i = 0; i < 10; i++)
    ;
  #pragma omp parallel shared (this)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp for linear (this)			// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  for (int i = 0; i < 10; i++)
    ;
  #pragma omp task depend(inout: this)		// { dg-error ".this. is not lvalue expression nor array section in .depend. clause" }
    ;
  #pragma omp task depend(inout: this[0])
    ;
  #pragma omp task affinity(this)		// { dg-error ".this. is not lvalue expression nor array section in .affinity. clause" }
    ;
  #pragma omp task affinity(this[0])
    ;
  #pragma omp parallel private (this)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  {
    #pragma omp single copyprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  }
}

template <int N>
struct T
{
  #pragma omp declare simd linear(this)	// { dg-error "invalid use of .this" }
  static void foo ();
  void bar ();
};

template <int N>
void
T<N>::bar ()
{
  #pragma omp parallel firstprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp parallel for lastprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  for (int i = 0; i < 10; i++)
    ;
  #pragma omp parallel shared (this)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp for linear (this)			// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  for (int i = 0; i < 10; i++)
    ;
  #pragma omp task depend(inout: this)		// { dg-error ".this. is not lvalue expression nor array section in .depend. clause" }
    ;
  #pragma omp task depend(inout: this[0])
    ;
  #pragma omp task affinity(this)		// { dg-error ".this. is not lvalue expression nor array section in .affinity. clause" }
    ;
  #pragma omp task affinity(this[0])
    ;
  #pragma omp parallel private (this)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  {
    #pragma omp single copyprivate (this)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  }
}

template struct T<0>;
