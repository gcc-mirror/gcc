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
  struct S a, b[10];
  #pragma omp parallel reduction(+:a)
    baz (&a);
}
