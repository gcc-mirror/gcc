// PR c++/58583
// { dg-do compile { target c++11 } }

template<int> struct A
{
  int i = (A<0>(), 0); // { dg-error "recursive instantiation of default" }
};

A<0> a;

template<int N> struct B
{
  B* p = new B<N>; // { dg-error "recursive instantiation of default" }
};

B<1> x;

struct C
{
  template<int N> struct D
  {
    D* p = new D<0>;
  };
};
