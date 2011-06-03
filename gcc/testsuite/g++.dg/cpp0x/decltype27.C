// PR c++/48617
// { dg-options -std=c++0x }

template<class T, decltype(T())> // #
struct A {};

A<int, 0> a;

int main() {}
