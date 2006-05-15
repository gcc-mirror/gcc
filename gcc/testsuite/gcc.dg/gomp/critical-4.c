/* { dg-do compile } */

extern void bar(int);

void
foo1 (void)
{
  #pragma omp critical
  #pragma omp critical(foo)
  #pragma omp critical(bar)
    bar (0);
}

void
foo2 (void)
{
  #pragma omp critical
  #pragma omp critical		/* { dg-warning "with the same name" } */
    bar (0);
}

void
foo3 (void)
{
  #pragma omp critical(foo)
  #pragma omp critical(foo)	/* { dg-warning "with the same name" } */
    bar (0);
}
