/* PR c/67500 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare simd simdlen(d)	/* { dg-error "clause expression must be positive constant integer expression" } */
void f1 (int);				/* { dg-error "undeclared here" "" { target *-*-* } .-1 } */
#pragma omp declare simd simdlen(0.5)	/* { dg-error "clause expression must be positive constant integer expression" } */
void f2 (int);
#pragma omp declare simd simdlen(-2)	/* { dg-error "clause expression must be positive constant integer expression" } */
void f3 (int);
#pragma omp declare simd simdlen(0)	/* { dg-error "clause expression must be positive constant integer expression" } */
void f4 (int);

void
foo (int *p)
{
  int i;
  #pragma omp simd safelen(d)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)		/* { dg-error "undeclared" "" { target *-*-* } .-1 } */
    ;
  #pragma omp simd safelen(0.5)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd safelen(-2)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd safelen(0)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd aligned(p:d)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd aligned(p:0.5)	/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd aligned(p:-2)	/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
  #pragma omp simd aligned(p:0)		/* { dg-error "must be positive constant integer expression" } */
  for (i = 0; i < 16; ++i)
    ;
}
