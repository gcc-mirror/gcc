// PR c++/93211
// { dg-do compile { target c++11 } }

template<typename T>
void g(T);

template<typename T, decltype(g(T{})) = 0>
void f() {}

template<typename T, decltype(::g(T{})) = 0>
void f() {} // { dg-bogus "redefinition" }
