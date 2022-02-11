// { dg-do compile }
// { dg-options "-fopenmp" }

struct S
{
  void bar (int);
};

void
S::bar (int x)
{
  #pragma omp target map (this, x)		// { dg-error "cannot take the address of .this., which is an rvalue expression" }
    ;
  #pragma omp target map (this[0], x)
    ;
  #pragma omp target update to (this, x)	// { dg-error "cannot take the address of .this., which is an rvalue expression" }
  #pragma omp target update to (this[0], x)
  #pragma omp target update from (this, x)	// { dg-error "cannot take the address of .this., which is an rvalue expression" }
  #pragma omp target update from (this[1], x)
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
  #pragma omp target map (this, x)		// { dg-error "cannot take the address of .this., which is an rvalue expression" }
    ;
  #pragma omp target map (this[0], x)
    ;
  #pragma omp target update to (this, x)	// { dg-error "cannot take the address of .this., which is an rvalue expression" }
  #pragma omp target update to (this[0], x)
  #pragma omp target update from (this, x)	// { dg-error "cannot take the address of .this., which is an rvalue expression" }
  #pragma omp target update from (this[1], x)
}

template struct T<0>;
