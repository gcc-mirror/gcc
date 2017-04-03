// PR c++/79533

struct S {
  S();
  S(const S&);
};
S f();
S s(static_cast<S const &>(f()));

// The static_cast prevents copy elision.
// { dg-final { scan-assembler "_ZN1SC1ERKS_" } }
