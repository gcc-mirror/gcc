// PR c++/90950
// { dg-do compile }

template <typename T>
T
foo (void)
{
  T y = 0;
  T &x = y;
  #pragma omp parallel for lastprivate (x)
  for (int i = 0; i < 8; ++i)
    x = i;
  return x;
}

int a = foo<int> ();
