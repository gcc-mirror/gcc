// PR c++/123597
// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not " shared\\\(sum\\\)" "gimple" } }

template <typename T>
void
foo (T *x)
{
  #pragma omp teams distribute parallel for collapse(2)
  for (long i = 0; i < 5; i++)
    for (long j = 0; j < 5; j++)
      {
        T sum = 0;
        for (long k = 0; k < 5; k++)
          sum += 2;
        x[i + 5 * j] = sum;
      }
}

void
bar (int *x)
{
  foo (x);
}
