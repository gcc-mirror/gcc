/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct S {};
void foo (void *, void *);
void bar (void *, void *);
void baz (void *);
#pragma omp declare reduction(+:struct S:foo (&omp_out, &omp_in))initializer(bar(&omp_priv, &omp_orig))

void
test1 (void)
{
  struct S s;
  int i;
  #pragma omp parallel reduction(+:s)
    baz (&s);
  #pragma omp parallel reduction(task, +:s)	/* { dg-error "zero sized type 'struct S' in 'reduction' clause" } */
    baz (&s);
  #pragma omp taskloop reduction(+:s)		/* { dg-error "zero sized type 'struct S' in 'reduction' clause" } */
  for (i = 0; i < 1; i++)
    baz (&s);
  #pragma omp taskloop simd reduction(+:s)	/* { dg-error "zero sized type 'struct S' in 'reduction' clause" } */
  for (i = 0; i < 1; i++)
    baz (&s);
  #pragma omp taskgroup task_reduction(+:s)	/* { dg-error "zero sized type 'struct S' in 'task_reduction' clause" } */
  {
    #pragma omp task in_reduction(+:s)		/* { dg-error "zero sized type 'struct S' in 'in_reduction' clause" } */
    baz (&s);
  }
}
