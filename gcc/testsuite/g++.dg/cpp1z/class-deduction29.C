// PR c++/79500
// { dg-options -std=c++1z }

template<typename T> struct A {};
A(...) -> A<int>;
A a = {};
