// PR c++/60267
// { dg-do compile }
// { dg-options "-O3" }

template <int N>
void
foo (int *a, int *b, int *c)
{
#pragma GCC ivdep
  for (int i = 0; i < N; i++)
    a[i] = b[i] * c[i];
}

void
bar (int *a, int *b, int *c)
{
  foo <64> (a, b, c);
}
