// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++20 } }

// Test that we don't accept designated inits in ( ).

struct S {
  int i;
  int j = 42;
};

S s(.i = 12); // { dg-error "expected" }

int a[]([0] = 42); // { dg-error "" }
