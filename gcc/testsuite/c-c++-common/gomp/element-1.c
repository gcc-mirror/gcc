/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct S { int a; };

void
foo (struct S *x)
{
  struct S b;
  #pragma omp parallel private (b.a)	/* { dg-error "expected .\\). before .\\.. token" } */
  ;
  #pragma omp parallel private (x->a)	/* { dg-error "expected .\\). before .->. token" } */
  ;
}
