// PR bootstrap/66448
// { dg-options "-Wunused-function" }

struct A { A(); };
namespace {
  struct B: virtual A { B(); };
  B::B() { }
  B b;

  struct C: virtual A { C(); };
  C::C() { }			// { dg-warning "defined but not used" }
}
