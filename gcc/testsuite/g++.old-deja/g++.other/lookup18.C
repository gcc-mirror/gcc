// Test that referring to an ambiguous base in name lookup prevents
// access to the field, even though the field is not ambiguous.

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
  B::i = 0;			// ERROR - B is ambiguous
}

void f () {
  E e;
  e.B::i = 0;			// ERROR - B is ambiguous
}
