/* Test parsing of #pragma omp declare simd */
/* { dg-do compile } */

#pragma omp declare simd
int a;	/* { dg-error "not immediately followed by a function declaration or definition" } */

#pragma omp declare simd
int fn1 (int a), fn2 (int a);	/* { dg-error "not immediately followed by a single function declaration or definition" } */

#pragma omp declare simd
int b, fn3 (int a);	/* { dg-error "not immediately followed by a function declaration or definition" } */

#pragma omp declare simd linear (a)
int fn4 (int a), c;	/* { dg-error "not immediately followed by a function declaration or definition" } */

int t;

#pragma omp declare simd
#pragma omp declare simd
#pragma omp threadprivate(t)	/* { dg-error "must be followed by function declaration or definition or another" } */
int fn5 (int a);

#pragma omp declare simd inbranch notinbranch /* { dg-error "clause is incompatible with" } */
int fn6 (int);

#pragma omp declare simd aligned (a, b)
int fn7 (int *a, int b[64]);

#pragma omp declare simd aligned (a)    /* { dg-error "neither a pointer nor an array" } */
int fn8 (int a);

#pragma omp declare simd aligned (c)    /* { dg-error "neither a pointer nor an array" } */
int fn9 (float c);

#pragma omp declare simd aligned (d)    /* { dg-error "neither a pointer nor an array" } */
int fn10 (double d);

struct D { int d; };

#pragma omp declare simd aligned (e)    /* { dg-error "neither a pointer nor an array" } */
int fn11 (struct D e);   

#pragma omp declare simd linear(a:7) uniform(a)	/* { dg-error "appears more than once" } */
int f12 (int a);
#pragma omp declare simd linear(a) linear(a)	/* { dg-error "appears more than once" } */
int f13 (int a);
#pragma omp declare simd linear(a) linear(a:7)	/* { dg-error "appears more than once" } */
int f14 (int a);
#pragma omp declare simd linear(a:6) linear(a:6)/* { dg-error "appears more than once" } */
int f15 (int a);
#pragma omp declare simd uniform(a) uniform(a)	/* { dg-error "appears more than once" } */
int f16 (int a);
#pragma omp declare simd uniform(a) aligned (a: 32)
int f17 (int *a);
