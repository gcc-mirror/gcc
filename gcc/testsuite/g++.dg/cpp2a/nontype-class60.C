// PR c++/99631
// { dg-do compile { target c++20 } }

struct A { };

template<auto V>
void f() {
  static_assert(__is_same(decltype(V), A));
}

template<class T, T V>
void g() {
  static_assert(__is_same(decltype(V), A));
}

constexpr A a;
template void f<a>();
template void g<A, A{}>();
