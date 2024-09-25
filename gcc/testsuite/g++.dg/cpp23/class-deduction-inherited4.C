// { dg-do compile { target c++23 } }

template<class T>
struct A { A(T); };

template<class T>
struct B : A<T> {
  using B::A::A; // FIXME: we don't notice this inherited ctor
};

using ty1 = decltype(B(0)); // { dg-bogus "" "" { xfail *-*-* } }
using ty1 = B<int>;

template<class T=void>
struct C : A<int> {
  using A<int>::A;
};

using ty2 = decltype(C(0));
using ty2 = C<void>;

template<class T>
struct D : A<T> {
  using A<T>::A;
};

using ty3 = decltype(D(0));
using ty3 = D<int>;

A(int) -> A<char>; // FIXME: we need to rebuild the guides of D
using ty4 = decltype(D(0));
using ty4 = D<char>; // { dg-bogus "conflicting" "" { xfail *-*-* } }
