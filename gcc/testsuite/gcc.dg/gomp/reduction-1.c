/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct S {};
void foo (void *, void *);
void bar (void *, void *);
void baz (void *);
#pragma omp declare reduction(+:struct S:foo (&omp_out, &omp_in))initializer(bar(&omp_priv, &omp_orig))

void
test (void)
{
  struct S b[10];
  #pragma omp parallel reduction(+:b[0:5])	/* { dg-error "zero length array section" } */
    baz (b);
  #pragma omp parallel reduction(+:b[:10])	/* { dg-error "zero length array section" } */
    baz (b);
  #pragma omp parallel reduction(+:b)		/* { dg-error "is a zero size array" } */
    baz (b);
}
