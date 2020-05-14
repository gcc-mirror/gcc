// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++20 } }

struct A;

struct C {
  operator A();
};

struct A {
  C c;
};

C c;
A a(c);  // invokes C’s conversion function to A

// { dg-final { scan-assembler "_ZN1Ccv1AEv" } }
