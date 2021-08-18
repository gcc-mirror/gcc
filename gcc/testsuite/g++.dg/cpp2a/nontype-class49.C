// PR c++/101883
// { dg-do compile { target c++20 } }

template<class T> struct C { constexpr C(int) { } };
explicit C(int) -> C<int>;

template<C c> struct X { };
X<1> x; // { dg-error "deduction|no match" }
