// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

// Test that we don't perform lifetime extension for () init.

struct A {
  int a;
  int&& r;
};

int f();
A a(1, f());

// { dg-final { scan-assembler-not "_ZGR1a" } }
