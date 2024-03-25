// PR c++/16240
// { dg-options "-fabi-version=2 -fabi-compat-version=2 -Wno-abi" }

void foo(char);
template<void (&)(char)> struct CB {};

void g(CB<foo> i) {}

// { dg-final { scan-assembler "\n_?_Z1g2CBILZ3foocEE\[: \t\n\]" } }
