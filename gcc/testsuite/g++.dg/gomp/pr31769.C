// PR tree-optimization/31769
// { dg-options "-O2 -fopenmp" }
// { dg-do compile }

struct B
{
  B () {}
  virtual ~B () {}
};
struct C
{
  C (int x, int y) {}
};
template<typename T, int U>
struct D
{
  D () {}
  ~D () {}
};
struct E
{
  E () {}
  ~E () {}
  D<int, 1> e;
};
struct A
{
  B *b;
  A () { b = __null; }
  ~A () { if (b != __null) delete b; }
};
struct F : public A
{
  explicit F (int x) { foo (0); }
  F (const F &x) {}
  F (F &x, C y) {}
  F operator () (C x) const
  {
    return F (const_cast<F &>(*this), x);
  }
  template <typename U> F & operator+= (const U &);
  void foo (int);
  E f;
};

int
main ()
{
  try
  {
    F f (10);
    F g (10);
    C h (0, 9);
#pragma omp parallel for
    for (int i = 0; i < 2; ++i)
      g += f (h);
  }
  catch (int &e)
  {
  }
}
