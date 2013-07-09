// PR c++/51786
// { dg-do compile { target c++11 } }

enum E {};
struct A {};

void foo() { decltype(E{}); }  // { dg-error "does not declare anything" }
void bar() { decltype(A{}); }  // { dg-error "does not declare anything" }
