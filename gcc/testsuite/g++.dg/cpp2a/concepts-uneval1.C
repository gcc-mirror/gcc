// PR c++/99961
// { dg-do compile { target c++20 } }

struct A { static const int x = 42; bool y; };

void f(auto a) requires (a.x == 42) { }
template void f(A);

template <bool V> concept C = V || A::y;
static_assert(C<true>);
