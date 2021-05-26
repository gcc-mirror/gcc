// { dg-do compile }
// { dg-options "-fopenmp" }

struct S
{
  void bar (int);
};

void
S::bar (int x)
{
  #pragma omp target map (this, x)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp target map (this[0], x)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp target update to (this, x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update to (this[0], x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update from (this, x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update from (this[1], x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
}

template <int N>
struct T
{
  void bar (int);
};

template <int N>
void
T<N>::bar (int x)
{
  #pragma omp target map (this, x)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp target map (this[0], x)		// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
    ;
  #pragma omp target update to (this, x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update to (this[0], x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update from (this, x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
  #pragma omp target update from (this[1], x)	// { dg-error ".this. allowed in OpenMP only in .declare simd. clauses" }
}

template struct T<0>;
