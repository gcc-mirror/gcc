// { dg-do assemble  }
// g++ 1.36.1 bug 900214_01

// g++ allows function members of incomplete types to be declared to be
// friends of other types.

// Cfront 2.0 passes this test.

// keywords: friends, incomplete types, function members

struct A;                       // { dg-error "" } forward declaration

struct B {
  friend void A::foo();		// { dg-error "" } type A is incomplete
};

void A::foo();			/* { dg-error "" } also illegal */

struct A {
  void foo() {}
};

int main () { return 0; }
