/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23 -Wno-attributes" } */

void
foo ()
{
  [[omp::directive (parallel)]] __asm ("");
  __extension__ [[omp::directive (parallel)]] asm ("");		/* { dg-error "expected" } */
  [[omp::directive (parallel)]] __label__ foo;			/* { dg-error "expected" } */
  [[omp::directive (parallel)]] static_assert (true, "");	/* { dg-error "expected" } */
  [[omp::directive (parallel)]] int a = 5;			/* { dg-error "not allowed to be specified in this context" } */
  int b = 0;
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wattributes"
  [[omp::directive (parallel)]] l: b++;				/* { dg-warning "'omp::directive' scoped attribute directive ignored" } */
  switch (0)
    {
      [[omp::directive (parallel)]] case 6: break;		/* { dg-warning "'omp::directive' scoped attribute directive ignored" } */
      [[omp::directive (parallel)]] default: break;		/* { dg-warning "'omp::directive' scoped attribute directive ignored" } */
    }
#pragma GCC diagnostic pop
}

void
bar ()
{
  [[omp::directive (declare simd)]] int a;		/* { dg-error "not allowed to be specified in this context|not immediately followed by a function declaration or definition" } */
  [[omp::directive (declare simd)]] int b, f1 (int);	/* { dg-error "not allowed to be specified in this context|not immediately followed by a function declaration or definition" } */
  [[omp::directive (declare simd)]] int f2 (int), c;	/* { dg-error "not immediately followed by a function declaration or definition" } */
  int d [[omp::directive (declare simd)]];		/* { dg-error "not immediately followed by a function declaration or definition" } */
  int f3 [[omp::directive (declare simd)]] (int), f4 [[omp::directive (declare simd)]] (int);
  __extension__ [[omp::directive (declare simd)]] int f5 (int);
  __extension__ int f6 [[omp::directive (declare simd, notinbranch)]] (int);
  #pragma omp declare simd notinbranch
  [[omp::directive (declare simd inbranch)]] int f7 (int);	/* { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same declaration" } */
  [[omp::directive (declare simd notinbranch)]]
  #pragma omp declare simd inbranch	/* { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" } */
  int f8 (int);
  static int t1, t2, t3, t4;
  [[omp::directive (declare simd), omp::directive (foobar)]] int f9 (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
  [[omp::directive (foobar), omp::directive (declare simd)]] int f10 (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
  [[omp::directive (threadprivate (t1)), omp::directive (declare simd)]] int f10 (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
  [[omp::directive (declare simd), omp::directive (threadprivate (t2))]] int f11 (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
  int f12 [[omp::directive (declare simd), omp::directive (foobar)]] (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
  int f13 [[omp::directive (foobar), omp::directive (declare simd)]] (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
  int f14 [[omp::directive (threadprivate (t3)), omp::directive (declare simd)]] (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
  int f15 [[omp::directive (declare simd), omp::directive (threadprivate (t4))]] (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
}

[[omp::directive (declare simd)]] int a;		/* { dg-error "not allowed to be specified in this context|not immediately followed by a function declaration or definition" } */
[[omp::directive (declare simd)]] int b, f16 (int);	/* { dg-error "not allowed to be specified in this context|not immediately followed by a function declaration or definition" } */
[[omp::directive (declare simd)]] int f17 (int), c;	/* { dg-error "not immediately followed by a function declaration or definition" } */
int d [[omp::directive (declare simd)]];		/* { dg-error "not immediately followed by a function declaration or definition" } */
int f18 [[omp::directive (declare simd)]] (int), f19 [[omp::directive (declare simd)]] (int);
__extension__ [[omp::directive (declare simd)]] int f20 (int);
__extension__ int f21 [[omp::directive (declare simd, notinbranch)]] (int);
#pragma omp declare simd notinbranch
[[omp::directive (declare simd inbranch)]] int f22 (int);	/* { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same declaration" } */
[[omp::directive (declare simd notinbranch)]]
#pragma omp declare simd inbranch	/* { dg-error "before '#pragma'" } */
int f23 (int);				/* { dg-error "not immediately followed by a function declaration or definition" "" { target *-*-* } .-1 } */
int t5, t6, t7, t8;
[[omp::directive (declare simd), omp::directive (foobar)]] int f24 (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
[[omp::directive (foobar), omp::directive (declare simd)]] int f25 (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
[[omp::directive (threadprivate (t5)), omp::directive (declare simd)]] int f26 (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
[[omp::directive (declare simd), omp::directive (threadprivate (t6))]] int f27 (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
int f28 [[omp::directive (declare simd), omp::directive (foobar)]] (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
int f29 [[omp::directive (foobar), omp::directive (declare simd)]] (int);	/* { dg-error "unknown OpenMP directive name" } */
										/* { dg-error "'omp::directive' not allowed to be specified in this context" "" { target *-*-* } .-1 } */
int f30 [[omp::directive (threadprivate (t7)), omp::directive (declare simd)]] (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */
int f31 [[omp::directive (declare simd), omp::directive (threadprivate (t8))]] (int);	/* { dg-error "'omp::directive' not allowed to be specified in this context" } */

void
baz ()
{
  #pragma omp parallel
  [[omp::directive (declare simd)]];				/* { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" } */
  [[omp::directive (parallel)]]
  #pragma omp declare simd	/* { dg-error "mixing OpenMP directives with attribute and pragma syntax on the same statement" } */
  extern int f34 (int);
}
