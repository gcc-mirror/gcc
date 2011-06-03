// PR c++/49276
// { dg-do compile }
// { dg-options "-std=c++0x" }

template <int N>
struct F
{
  template <typename U> F (U);
};

struct S
{
  void foo (F <0> x = [] {}) {}
};

int
main ()
{
  S s;
  s.foo ();
}
