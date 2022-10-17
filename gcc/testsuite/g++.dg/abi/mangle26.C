// Test of std mangling

// { dg-do compile }
// { dg-options "-fno-inline -fno-implicit-constexpr" }

namespace std {
  struct A {
    A() { }
  };
}

std::A a;

// { dg-final { scan-assembler "\n_?_ZNSt1AC\[12\]Ev\[: \t\n\]" } }
