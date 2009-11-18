// Test of std mangling

// { dg-do compile }
// { dg-options "-fno-inline" }

namespace std {
  struct A {
    virtual void f () { }
  };
}

std::A a;

// { dg-final { scan-assembler "\n_?_ZNSt1AC\[12\]Ev\[: \t\n\]" } }
