// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

struct B { };
struct A : B {
  int i;
};

B b;
A a(b);
