// Test that referring to an ambiguous base in name lookup does not
// interfere with accessing the field, which is not ambiguous.

// Build don't link:

struct A {
  int i;
};
struct B: virtual A { };
struct C: public B { };
struct D: public B { };
struct E: public C, public D {
  void f ();
};

void E::f() {
  B::i = 0;
}

void f () {
  E e;
  e.B::i = 0;
}
