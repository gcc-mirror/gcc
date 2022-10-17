// PR c++/102854
// { dg-do compile }

template <typename T>
void
foo (T N, T M)
{
  #pragma omp parallel for collapse(2)
  for (T i = 0; i < N; ++i)
    for (T k = i; k < M; ++k)
      ;
  #pragma omp parallel for collapse(2)
  for (T i = 0; i < N; ++i)
    for (T k = i; k < 2 * i; ++k)
      ;
}

void
bar ()
{
  foo (5, 10);
}
