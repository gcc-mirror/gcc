// PR c++/88976
// { dg-do compile }

template <class T> void
foo (T x)
{
#pragma omp parallel
  {
  #pragma omp cancel parallel if (x)
  }
#pragma omp parallel
  {
  #pragma omp cancel parallel if (1 == 1)
  }
}

void
bar (int x, double y, long long z)
{
  foo (0);
  foo (1LL);
  foo (1.25);
  foo (x);
  foo (y);
  foo (z);
}
