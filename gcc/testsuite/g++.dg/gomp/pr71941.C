// PR c++/71941
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A { A (); A (A &); ~A (); };

template <int N>
struct B
{
  struct C { A a; C () : a () {} };
  C c;
  void foo ();
};

void
bar ()
{
  B<0> b;
#pragma omp task
  for (int i = 0; i < 2; i++)
    b.foo ();
}
