// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58637

template<> void foo(auto); // { dg-error "auto|not a template" }

