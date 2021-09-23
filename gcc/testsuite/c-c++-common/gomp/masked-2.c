void bar (void);
struct S { int s; };

void
foo (float f, struct S s)
{
  #pragma omp masked filter (0.0)	/* { dg-error "integral|integer" } */
  bar ();
  #pragma omp masked filter (s)		/* { dg-error "integral|integer" } */
  bar ();
}
