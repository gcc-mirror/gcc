/* { dg-do compile } */
/* { dg-options "-fopenmp-simd" } */

#pragma omp declare simd linear(a:1 + b) uniform(b)	/* { dg-error ".linear. clause step .b \\+ 1. is neither constant nor a parameter" } */
int f1 (int a, int b);
#pragma omp declare simd linear(a:b + 1) uniform(b)	/* { dg-error ".linear. clause step .b \\+ 1. is neither constant nor a parameter" } */
int f2 (int a, int b);
#pragma omp declare simd linear(a:2 * b) uniform(b)	/* { dg-error ".linear. clause step .b \\* 2. is neither constant nor a parameter" } */
int f3 (int a, int b);
#pragma omp declare simd linear(a:b)			/* { dg-error ".linear. clause step is a parameter .b. not specified in .uniform. clause" } */
int f4 (int a, int b);
#pragma omp declare simd linear(a:b) linear(b:1)	/* { dg-error ".linear. clause step is a parameter .b. not specified in .uniform. clause" } */
int f5 (int a, int b);
#pragma omp declare simd linear(a:5 + 2 * 3)
int f6 (int a, int b);
const int c = 5;
#pragma omp declare simd linear(a:c)			/* { dg-error ".linear. clause step .c. is neither constant nor a parameter" } */
int f7 (int a, int b);
#pragma omp declare simd linear(a:2 * c + 1)		/* { dg-error ".linear. clause step .\[^\n\r]*. is neither constant nor a parameter" } */
int f8 (int a, int b);
#pragma omp declare simd linear(a:0.5)			/* { dg-error ".linear. clause step expression must be integral" } */
int f9 (int a, int b);
