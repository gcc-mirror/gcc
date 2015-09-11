// { dg-do compile }

template <typename T>
void f(T A, T B)
{
  extern int *v;
  T a = 2;
  T b = 4;

#pragma omp target update to(v[a:b])
  v[a] = 0;

#pragma omp target update to(v[A:B])
  v[a] = 0;
}

void g()
{
  f(1, 5);
}
