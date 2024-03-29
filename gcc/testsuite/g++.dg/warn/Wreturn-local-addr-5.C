// PR c++/86982
// { dg-do compile { target c++11 } }

#include <utility>

int&& f() { int i = 0; return std::move(i); } // { dg-warning "reference to local variable" }
int&& g() { int i = 0; return std::forward<int>(i); } // { dg-warning "reference to local variable" }
int&& h() { long l = 0; return std::forward<int>(l); } // { dg-warning "reference to temporary" "" { target { ! c++26 } } }
// { dg-error "reference to temporary" "" { target c++26 } .-1 }
