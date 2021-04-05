// PR c++/99586
// { dg-do compile { target c++20 } }

template <class T>
struct B { constexpr B(T); };

template <auto> struct A{};
template <auto V> auto a = A<B{V}>{};
