// { dg-do compile { target c++11 } }

using nullptr_t = decltype(nullptr);

constexpr nullptr_t n = nullptr;

template <void *> struct A { };

A<n> a;
