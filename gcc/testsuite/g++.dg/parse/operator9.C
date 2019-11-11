// { dg-do compile }

struct A {};
struct B {};
int operator?:(bool, A, B);  // { dg-error "prohibits overloading" }
