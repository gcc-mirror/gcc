// PR c++/79500
// { dg-options -std=c++17 }

template<typename T> struct A {};
A(...) -> A<int>;
A a = {};
