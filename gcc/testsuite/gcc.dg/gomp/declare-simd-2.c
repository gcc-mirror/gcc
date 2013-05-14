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
