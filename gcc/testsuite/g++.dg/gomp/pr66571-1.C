// PR c++/66571
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T, typename U>
extern void bar (T, T, U);

template <typename T, typename U>
void
foo (T a, T b, U c)
{
  #pragma omp parallel for simd shared (a, c) reduction (+:b)
  for (int i = 0; i < 10; i++)
    bar<T> (a, b, c);
  #pragma omp target map(tofrom:a, c[0:5])
    ;
  #pragma omp task depend(inout:c[4:2])
    ;
  T d = a;
  T e = b;
  U f = c;
  #pragma omp parallel for simd shared (d, f) reduction (+:e)
  for (int i = 0; i < 10; i++)
    bar<T> (d, e, f);
  #pragma omp target map(tofrom:d, f[0:5])
    ;
  #pragma omp task depend(inout:f[4:2])
    ;
}

void
baz ()
{
  int a = 0, b = 0, cb[10] = {}, *c = cb;
  foo <int, int *> (a, b, c);
  foo <int &, int *&> (a, b, c);
}
