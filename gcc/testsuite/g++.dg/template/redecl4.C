// PR c++/28710
// { dg-do compile }

template<int> union A;  // { dg-error "previous" }
struct A;               // { dg-error "non-template" }
