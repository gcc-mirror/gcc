// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++20 } }

struct A {
  int i[2];
};

A a({1});
A a2({1, 2});
A a3(1); // { dg-error "array must be initialized with a brace-enclosed initializer" }
A a4 = A(1); // { dg-error "array must be initialized with a brace-enclosed initializer" }
A a5 = A({1});
