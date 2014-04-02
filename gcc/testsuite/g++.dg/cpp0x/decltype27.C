// PR c++/48617
// { dg-do compile { target c++11 } }

template<class T, decltype(T())> // #
struct A {};

A<int, 0> a;

int main() {}
