/* PR middle-end/98205 */

void baz (int) __attribute__((noreturn));

void
foo (int n)
{
  int i;
  #pragma omp for ordered(1)
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered depend(source)
      #pragma omp ordered depend(sink: i - 2)
      baz (i);
    }
}

void
bar (int n)
{
  int i, j;
  #pragma omp for collapse(2) ordered(2)
  for (i = 0; i < 8; i += n)
    for (j = 0; j < 8; j += n)
      {
        #pragma omp ordered depend(source)
        #pragma omp ordered depend(sink: i - 2, j + 2)
        baz (i);
      }
}
