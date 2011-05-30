// PR c++/49223
// { dg-do compile }
// { dg-options "-fopenmp" }

template <int N>
struct V
{
  V () {}
  ~V () {}
};

template <int N>
struct S
{
  void foo ()
  {
    V <0> v;
    #pragma omp parallel private (v)
      ;
  }
};

void
bar (void)
{
  S <0> s;
  s.foo ();
}
