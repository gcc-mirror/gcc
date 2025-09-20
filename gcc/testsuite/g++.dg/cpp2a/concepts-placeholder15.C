// PR c++/121981
// { dg-do compile { target c++20 } }

template<auto V>
concept C = requires { V; };

template<class T, auto V>
concept D = C<V>;

template<auto V, D<V> auto W>
struct A { };

template<auto V, D<V> T>
struct B { };

A<0, 1> a;
B<0, int> b;
