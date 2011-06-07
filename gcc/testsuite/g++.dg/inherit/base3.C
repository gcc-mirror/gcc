// PR c++/35985
// { dg-do compile }

template<typename T> struct A : T {};  // { dg-error "struct or class type" }

struct B;

A<void (B::*)()> a;  // { dg-message "required" }
