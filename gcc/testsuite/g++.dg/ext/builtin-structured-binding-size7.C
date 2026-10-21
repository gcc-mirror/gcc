// PR c++/127766
// { dg-do compile { target c++11 } }

template <typename T> struct A { T a, b; };
static_assert (__builtin_structured_binding_size (A <int>) == 2, "");

template <typename T> struct B { T a, b, c; };
template <typename T>
constexpr int foo () { return __builtin_structured_binding_size (B<T>); }
static_assert (foo <int> () == 3, "");
