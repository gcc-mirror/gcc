// PR c++/28710
// { dg-do compile }

template<int> union A;  // { dg-message "previous" }
struct A;               // { dg-error "non-template" }
