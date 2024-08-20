// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

struct C {};
struct B : virtual C {};

// Despite no non-inline key function, this is still a dynamic class
// and so by the Itanium ABI 5.2.3 should be uniquely emitted in this TU
export struct A : B {
  inline A (int) {}
};

// { dg-final { scan-assembler {_ZTTW1M1A:} } }
// { dg-final { scan-assembler {_ZTVW1M1A:} } }
