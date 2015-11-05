// { dg-do compile }
// { dg-options "-fopenmp" }

struct S { int a; void foo (S *); static S &bar (); };

void
S::foo (S *x)
{
  S &b = bar ();
  S c;
  #pragma omp parallel private (b.a)	// { dg-error "expected .\\). before .\\.. token" }
  ;
  #pragma omp parallel private (c.a)	// { dg-error "expected .\\). before .\\.. token" }
  ;
  #pragma omp parallel private (x->a)	// { dg-error "expected .\\). before .->. token" }
  ;
}
