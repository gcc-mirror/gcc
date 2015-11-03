// PR c++/66571
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T>
extern void bar (T, T, T, T);

template <typename T>
void
foo (T a, T b, T c, T d)
{
  #pragma omp parallel for simd private (a) firstprivate (b) lastprivate (c) linear (d:2)
  for (int i = 0; i < 10; i++)
    bar<T> (a, b, c, d), d += 2;
  #pragma omp parallel private (c)
    #pragma omp single copyprivate (c)
      bar<T> (a, b, c, d);
  T e = a;
  T f = b;
  T g = c;
  T h = d;
  #pragma omp parallel for simd private (e) firstprivate (f) lastprivate (g) linear (h:2)
  for (int i = 0; i < 10; i++)
    bar<T> (e, f, g, h), h += 2;
  #pragma omp parallel private (g)
    #pragma omp single copyprivate (g)
      bar<T> (e, f, g, h);
}

void
baz ()
{
  int a = 0, b = 0, c = 0, d = 0;
  foo <int> (a, b, c, d);
  foo <int &> (a, b, c, d);
}
