// PR tree-optimization/30558
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T> struct F
{
  ~F ();
  F (T);
  const T &operator[] (unsigned i) const;
};

template <typename T> F<T> foo (const F<T> &x)
{
  return F<T> (x[1]);
}

struct G
{
  G () { bar (2); }
  F<int> &operator () (F<int> x);
  void bar (int);
};

int
main ()
{
  try
  {
    G g;
#pragma omp parallel for
    for (int i = 0; i < 10; ++i)
      {
	F<int> j (i);
	F<int> f = g (j);
	F<int> h = foo (f);
      }
  }
  catch (int &e)
  {
  }
}
