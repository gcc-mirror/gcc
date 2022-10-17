// PR c++/82110
// { dg-do compile { target c++20 } }

struct X { X() = delete; };

template<class T> concept C = requires(T t) { new T; };
template<class T> concept D = requires(T t) { new T[1]; };

static_assert(!C<X>);
static_assert(!D<X>);
