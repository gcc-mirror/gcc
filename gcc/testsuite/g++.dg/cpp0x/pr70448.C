// { dg-do compile { target c++11 } }

template<int>
void f(){}

enum{n=f};  // { dg-error "enumerator value" }
