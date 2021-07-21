/* PR middle-end/101535 */

void
foo (void)
{
  int a = 1, i;
  #pragma omp target data map(to:a)
  #pragma omp for lastprivate(i)	/* { dg-error "lastprivate variable 'i' is private in outer context" } */
  for (i = 1; i < 2; i++)
    ;
}

void
bar (void)
{
  int a = 1, i;
  #pragma omp target private(i)
  #pragma omp for lastprivate(i)	/* { dg-error "lastprivate variable 'i' is private in outer context" } */
  for (i = 1; i < 2; i++)
    ;
}

void
baz (void)
{
  int a = 1, i;
  #pragma omp target firstprivate(i)
  #pragma omp for lastprivate(i)	/* { dg-error "lastprivate variable 'i' is private in outer context" } */
  for (i = 1; i < 2; i++)
    ;
}
