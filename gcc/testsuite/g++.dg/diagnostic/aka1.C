// Basic test for typedef stripping in diagnostics.

struct A {
  void f();
};

void A::f() {
  // We don't want an aka for the injected-class-name.
  A a = 0;			// { dg-error "type .A. requested" }
}

typedef A B;

// We do want an aka for a real typedef.
B b = 0;			// { dg-error "B .aka A." }
