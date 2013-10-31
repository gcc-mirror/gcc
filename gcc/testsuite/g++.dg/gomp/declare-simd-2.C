// Test parsing of #pragma omp declare simd
// { dg-do compile }

#pragma omp declare simd
int a;	// { dg-error "not immediately followed by function declaration or definition" }

#pragma omp declare simd
int fn1 (int a), fn2 (int a);	// { dg-error "not immediately followed by a single function declaration or definition" }

#pragma omp declare simd
int b, fn3 (int a);	// { dg-error "not immediately followed by function declaration or definition" }

#pragma omp declare simd linear (a)
int fn4 (int a), c;	// { dg-error "not immediately followed by function declaration or definition" }

#pragma omp declare simd
extern "C"		// { dg-error "not immediately followed by function declaration or definition" }
{
  int fn5 (int a);
}

#pragma omp declare simd // { dg-error "not immediately followed by function declaration or definition" }
namespace N1
{
  int fn6 (int a);
}

#pragma omp declare simd simdlen (4)
struct A
{			// { dg-error "not immediately followed by function declaration or definition" }
  int fn7 (int a);
};

#pragma omp declare simd
template <typename T>
struct B
{			// { dg-error "not immediately followed by function declaration or definition" }
  int fn8 (int a);
};

struct C
{
#pragma omp declare simd // { dg-error "not immediately followed by function declaration or definition" }
  public:		 // { dg-error "expected unqualified-id before" }
    int fn9 (int a);
};

int t;

#pragma omp declare simd
#pragma omp declare simd
#pragma omp threadprivate(t)	// { dg-error "not immediately followed by function declaration or definition" }
int fn10 (int a);

#pragma omp declare simd inbranch notinbranch // { dg-error "clause is incompatible with" }
int fn11 (int);

struct D
{
  int d;
  #pragma omp declare simd simdlen (N) linear (a : sizeof (e) + sizeof (this->e)) // { dg-error "was not declared" }
  template <int N>
  int fn12 (int a);
  int e;
};

#pragma omp declare simd aligned (a, b, c, d)
int fn13 (int *a, int b[64], int *&c, int (&d)[64]);

#pragma omp declare simd aligned (a)	// { dg-error "neither a pointer nor an array" }
int fn14 (int a);

#pragma omp declare simd aligned (b)	// { dg-error "neither a pointer nor an array" }
int fn14 (int &b);

#pragma omp declare simd aligned (c)	// { dg-error "neither a pointer nor an array" }
int fn14 (float c);

#pragma omp declare simd aligned (d)	// { dg-error "neither a pointer nor an array" }
int fn14 (double &d);

#pragma omp declare simd aligned (e)	// { dg-error "neither a pointer nor an array" }
int fn14 (D e);

// { dg-error "has no member" "" { target *-*-* } 61 }
