// PR c++/89356
// { dg-do compile { target c++11 } }

typedef unsigned a;
template <typename> struct h {};
template <int, class b> auto c(b f) -> h<decltype(f(a{0}))>;
typedef char byte;
enum d : byte;
d g(byte);
h<d> e = c<6>(g);
