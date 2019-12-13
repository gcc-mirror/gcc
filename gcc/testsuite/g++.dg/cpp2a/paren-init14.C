// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++11 } }

struct A {
  int x;
  int y;
  A(int, int) = delete;
};

A a(1, 2); // { dg-error "use of deleted function" }
