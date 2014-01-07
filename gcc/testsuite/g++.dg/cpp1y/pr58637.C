// PR c++/58637
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

template<> void foo(auto); // { dg-error "auto|not a template" }
