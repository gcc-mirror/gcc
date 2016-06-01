/* PR middle-end/71371 */
/* { dg-do compile } */

void baz (int *);

void
foo (void)
{
  int i;
  #pragma omp taskloop
  for (i = 0; i < 100; i++)
    baz (&i);
}

void
bar (void)
{
  int i;
  #pragma omp parallel
  {
    #pragma omp for
    for (i = 0; i < 100; i++)
      baz (&i);
  }
}
