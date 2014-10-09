// PR c++/58637
// { dg-do compile { target c++14 } }

template<> void foo(auto); // { dg-error "auto|not a template" }
