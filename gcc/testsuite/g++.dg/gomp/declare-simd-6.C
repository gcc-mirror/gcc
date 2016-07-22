// PR c++/71257
// { dg-do compile }
// { dg-options "-fopenmp-simd" }

struct S { int a; };
#pragma omp declare simd linear(val(a):2)
int f1 (int &a);
#pragma omp declare simd linear(uval(a):2)
unsigned short f2 (unsigned short &a);
#pragma omp declare simd linear(ref(a):1)
int f3 (long long int &a);
#pragma omp declare simd linear(a:1)
int f4 (int &a);
#pragma omp declare simd linear(val(a))
int f5 (int a);
#pragma omp declare simd linear(uval(a):2)		// { dg-error "modifier applied to non-reference variable" }
int f6 (unsigned short a);
#pragma omp declare simd linear(ref(a):1)		// { dg-error "modifier applied to non-reference variable" }
int f7 (unsigned long int a);
#pragma omp declare simd linear(a:1)
int f8 (int a);
#pragma omp declare simd linear(val(a):2)		// { dg-error "applied to non-integral non-pointer variable" }
int f9 (S &a);
#pragma omp declare simd linear(uval(a):2)		// { dg-error "applied to non-integral non-pointer variable" }
int f10 (S &a);
#pragma omp declare simd linear(ref(a):1)		// { dg-bogus "applied to non-integral non-pointer variable" }
int f11 (S &a);
#pragma omp declare simd linear(a:1)			// { dg-error "applied to non-integral non-pointer variable" }
int f12 (S &a);
#pragma omp declare simd linear(val(a))			// { dg-error "applied to non-integral non-pointer variable" }
int f13 (S a);
#pragma omp declare simd linear(uval(a):2)		// { dg-error "modifier applied to non-reference variable" }
int f14 (S a);
#pragma omp declare simd linear(ref(a):1)		// { dg-error "modifier applied to non-reference variable" }
int f15 (S a);
#pragma omp declare simd linear(a:1)			// { dg-error "applied to non-integral non-pointer variable" }
int f16 (S a);
