// PR c++/98352
// { dg-do compile }

struct A {
  int i = (A(), 42); // { dg-error "default member initializer" }
// { dg-error "only available" "" { target c++98_only } .-1 }
};
A a;
