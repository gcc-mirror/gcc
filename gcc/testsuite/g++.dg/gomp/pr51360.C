// PR c/51360
// { dg-do compile }
// { dg-options "-Wunused -W -fopenmp" }

template <typename T>
void
foo (T a, T b, T c)
{
  T m, n, o, i;
  m = 6;
  n = 1;
  o = 5;
  a = 6;
  b = 1;
  c = 5;
  #pragma omp parallel for num_threads (m) if (n) schedule (static, o)
  for (i = 0; i < 10; i++)
    ;
  #pragma omp parallel for num_threads (a) if (b) schedule (static, c)
  for (i = 0; i < 10; i++)
    ;
}

void
bar ()
{
  foo (0, 0, 0);
}
