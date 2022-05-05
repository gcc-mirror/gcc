// PR c++/105061

template <typename T, int U, int V>
struct A { T : V, u : U; };
template <int U, int V>
struct B { unsigned : V, u : U; };
typedef unsigned uns;
template <int U, int V>
struct C { uns : V, u : U; };

A<unsigned int, 4, 5> a = { 13 };
B<5, 6> b = { 26 };
C<8, 9> c = { 42 };
