// PR c++/79500
// { dg-do compile { target c++17 } }

template<typename T> struct A {};
A(...) -> A<int>;
A a = {};
