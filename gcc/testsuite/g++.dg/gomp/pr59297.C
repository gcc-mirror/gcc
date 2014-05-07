// PR c++/59297
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T>
struct A
{
  ~A ();
  const T &operator[] (int) const;
};

struct B
{
  int &operator () (A <int>);
};

void
foo (B &x, int &z)
{
  A<A<int> > y;
  #pragma omp atomic
  x (y[0]) += 1;
  #pragma omp atomic
  z += x(y[1]);
}
