// PR c++/67210
// { dg-do compile { target c++20 } }

template <class T, class U>
concept C = true;

template <class T>
struct A {};

void f(A<C<int> auto >) {} 
