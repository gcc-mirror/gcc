// { dg-do compile { target c++20 } }

template <auto V> concept C = false || V || false; // { dg-error "has type 'int'" }
template <auto V> int f() requires C<V>;
int a = f<0>(); // { dg-error "no match" }
