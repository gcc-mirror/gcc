// PR c++/94124 - wrong conversion error with non-viable overload.
// { dg-do compile { target c++11 } }

template <int N> struct A { typedef int _Type[N]; };
template <int N> struct B { typename A<N>::_Type _M_elems; };
class C { };
struct D {
  D(C);
};

struct F {
  F(B<2>);
  F(D); // This overload should not be viable.
};
F fn1() { return {{{0}}}; }
