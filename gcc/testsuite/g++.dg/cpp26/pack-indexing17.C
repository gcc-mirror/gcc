// PR c++/121795
// { dg-do compile { target c++26 } }

template<class T, class... Ts>
struct A;

template<class... Ts>
struct A<Ts...[sizeof...(Ts)-1], Ts...> { };

A<int, int> x;
A<char, int> y; // { dg-error "incomplete" }
