/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-additional-options "-std=c99" { target c } } */

struct S *p;	/* { dg-error "forward declaration" "" { target c++ } } */
float f;
int j;

void
foo (void)
{
#pragma omp simd linear(p) linear(f : 1)
  for (int i = 0; i < 10; i++)
    ;
#pragma omp simd linear(j : 7.0)	/* { dg-error "step expression must be integral" } */
  for (int i = 0; i < 10; i++)
    ;
}

/* { dg-error "linear clause applied to" "" { target *-*-* } 12 } */
/* { dg-error "(incomplete|undefined) type" "" { target *-*-* } 12 } */
