/* PR middle-end/27416 */
/* { dg-do compile } */

void
foo (void)
{
  int i = 0, j = 0;
#pragma omp for firstprivate (j) /* { dg-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j++;
}

int
bar (void)
{
  int i, j;
#pragma omp for lastprivate (j)	/* { dg-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j = i;
  return j;
}

int
baz (void)
{
  int i, j = 0;
#pragma omp for reduction (+:j)	/* { dg-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j++;
  return j;
}
