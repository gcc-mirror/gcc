// PR c/51360
// { dg-do compile }
// { dg-options "-Wunused -W -fopenmp" }

template <typename T>
void
foo (T a, T b, T c, T d)
{
  T m, n, o, p, i;
  m = 6;
  n = 1;
  o = 5;
  p = 1;
  a = 6;
  b = 1;
  c = 5;
  d = 1;
  #pragma omp parallel for num_threads (m) if (n) schedule (static, o)
  for (i = 0; i < 10; i++)
    ;
  #pragma omp parallel for num_threads (a) if (b) schedule (static, c)
  for (i = 0; i < 10; i++)
    ;
  #pragma omp task final (p)
    ;
  #pragma omp task final (d)
    ;
}

void
bar ()
{
  foo (0, 0, 0, 0);
}
