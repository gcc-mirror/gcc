/* PR c/97958 */

int *p;

void
foo (void)
{
  #pragma omp atomic
  p = p + 1;
}

void
bar (void)
{
  #pragma omp atomic	/* { dg-error "invalid expression type for '#pragma omp atomic'" } */
  bar = bar + 1;
}
