// PR c++/57947
// { dg-options "-std=c++98" }

namespace std
{
  template <class E> class initializer_list {};
  template <int N> struct D { D(initializer_list<int>) {} };
  D<0> d {1, 2, 3};  // { dg-error "constructor|no matching" }
  // { dg-warning "initializer list" "" { target *-*-* } .-1 }
}
