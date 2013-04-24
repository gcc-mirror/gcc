/* { dg-do compile { target { ! c } } } */
/* { dg-options "-fopenmp" } */
/* { dg-additional-options "-std=c99" { target c } } */

struct S *p;	/* { dg-error "forward declaration" } */
float f;
int j;

void
foo (void)
{
#pragma omp simd linear(p) linear(f : 1)
  for (int i = 0; i < 10; i++)
    ;
#pragma omp simd linear(j : 7.0)	/* { dg-error "linear step expression must be integral" } */
  for (int i = 0; i < 10; i++)
    ;
}

/* { dg-error "linear clause applied to" "" { target *-*-* } 12 } */
/* { dg-error "incomplete type" "" { target *-*-* } 12 } */
