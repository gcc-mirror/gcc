// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

// Test from [dcl.init].

struct A {
  int a;
  int&& r;
};

int f();
int n = 10;

A a1{1, f()};               // OK, lifetime is extended
A a2(1, f());               // well-formed, but dangling reference
A a3{1.0, 1};               // { dg-error "narrowing conversion" }
A a4(1.0, 1);               // well-formed, but dangling reference
A a5(1.0, static_cast<int&&>(n));    // OK
