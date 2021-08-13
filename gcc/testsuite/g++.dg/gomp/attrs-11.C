// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp -Wno-attributes" }

namespace N {}
namespace O { typedef int T; };

void
foo ()
{
  [[omp::directive (parallel)]] asm ("");			// { dg-error "expected" }
  [[omp::directive (parallel)]] __extension__ asm ("");		// { dg-error "expected" }
  __extension__ [[omp::directive (parallel)]] asm ("");		// { dg-error "expected" }
  [[omp::directive (parallel)]] namespace M = ::N;		// { dg-error "expected" }
  [[omp::directive (parallel)]] using namespace N;		// { dg-error "not allowed to be specified in this context" }
  [[omp::directive (parallel)]] using O::T;			// { dg-error "expected" }
  [[omp::directive (parallel)]] __label__ foo;			// { dg-error "expected" }
  [[omp::directive (parallel)]] static_assert (true, "");	// { dg-error "expected" }
  [[omp::directive (parallel)]] int a = 5;			// { dg-error "not allowed to be specified in this context" }
  int b = 0;
  [[omp::directive (parallel)]] l: b++;				// { dg-error "not allowed to be specified in this context" }
  switch (0)
    {
      [[omp::directive (parallel)]] case 6: break;		// { dg-error "not allowed to be specified in this context" }
      [[omp::directive (parallel)]] default: break;		// { dg-error "not allowed to be specified in this context" }
    }
}

void
bar ()
{
  [[omp::directive (declare simd)]] int a;		// { dg-error "not allowed to be specified in this context|not immediately followed by function declaration or definition" }
  [[omp::directive (declare simd)]] int b, f1 (int);	// { dg-error "not allowed to be specified in this context|not immediately followed by function declaration or definition" }
  [[omp::directive (declare simd)]] int f2 (int), c;	// { dg-error "not immediately followed by function declaration or definition" }
  int d [[omp::directive (declare simd)]];		// { dg-error "not allowed to be specified in this context" }
  int f3 [[omp::directive (declare simd)]] (int), f4 [[omp::directive (declare simd)]] (int);
  __extension__ [[omp::directive (declare simd)]] int f5 (int);
  __extension__ int f6 [[omp::directive (declare simd, notinbranch)]] (int);
  #pragma omp declare simd notinbranch
  [[omp::directive (declare simd inbranch)]] int f7 (int);	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same declaration" }
  [[omp::directive (declare simd notinbranch)]]
  #pragma omp declare simd inbranch	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  int f8 (int);
  static int t1, t2, t3, t4;
  [[omp::directive (declare simd), omp::directive (foobar)]] int f9 (int);	// { dg-error "unknown OpenMP directive name" }
  [[omp::directive (foobar), omp::directive (declare simd)]] int f10 (int);	// { dg-error "unknown OpenMP directive name" }
  [[omp::directive (threadprivate (t1)), omp::directive (declare simd)]] int f10 (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
  [[omp::directive (declare simd), omp::directive (threadprivate (t2))]] int f11 (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
  int f12 [[omp::directive (declare simd), omp::directive (foobar)]] (int);	// { dg-error "unknown OpenMP directive name" }
  int f13 [[omp::directive (foobar), omp::directive (declare simd)]] (int);	// { dg-error "unknown OpenMP directive name" }
  int f14 [[omp::directive (threadprivate (t3)), omp::directive (declare simd)]] (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
  int f15 [[omp::directive (declare simd), omp::directive (threadprivate (t4))]] (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
}

[[omp::directive (declare simd)]] int a;		// { dg-error "not allowed to be specified in this context|not immediately followed by function declaration or definition" }
[[omp::directive (declare simd)]] int b, f16 (int);	// { dg-error "not allowed to be specified in this context|not immediately followed by function declaration or definition" }
[[omp::directive (declare simd)]] int f17 (int), c;	// { dg-error "not immediately followed by function declaration or definition" }
int d [[omp::directive (declare simd)]];		// { dg-error "not allowed to be specified in this context" }
int f18 [[omp::directive (declare simd)]] (int), f19 [[omp::directive (declare simd)]] (int);
__extension__ [[omp::directive (declare simd)]] int f20 (int);
__extension__ int f21 [[omp::directive (declare simd, notinbranch)]] (int);
#pragma omp declare simd notinbranch
[[omp::directive (declare simd inbranch)]] int f22 (int);	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same declaration" }
[[omp::directive (declare simd notinbranch)]]		// { dg-error "'declare simd' directive not immediately followed by function declaration or definition" }
#pragma omp declare simd inbranch	// { dg-error "'#pragma' is not allowed here" }
int f23 (int);
int t5, t6, t7, t8;
[[omp::directive (declare simd), omp::directive (foobar)]] int f24 (int);	// { dg-error "unknown OpenMP directive name" }
[[omp::directive (foobar), omp::directive (declare simd)]] int f25 (int);	// { dg-error "unknown OpenMP directive name" }
[[omp::directive (threadprivate (t5)), omp::directive (declare simd)]] int f26 (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
[[omp::directive (declare simd), omp::directive (threadprivate (t6))]] int f27 (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
int f28 [[omp::directive (declare simd), omp::directive (foobar)]] (int);	// { dg-error "unknown OpenMP directive name" }
int f29 [[omp::directive (foobar), omp::directive (declare simd)]] (int);	// { dg-error "unknown OpenMP directive name" }
int f30 [[omp::directive (threadprivate (t7)), omp::directive (declare simd)]] (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }
int f31 [[omp::directive (declare simd), omp::directive (threadprivate (t8))]] (int);	// { dg-error "OpenMP directive other than 'declare simd' or 'declare variant' appertains to a declaration" }

void
baz ()
{
  #pragma omp parallel
  [[omp::directive (declare simd)]] extern int f32 (int);	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  #pragma omp parallel
  extern int f33 [[omp::directive (declare simd)]] (int);	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  [[omp::directive (parallel)]]
  #pragma omp declare simd	// { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" }
  extern int f34 (int);
}
