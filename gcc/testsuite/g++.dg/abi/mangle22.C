// PR c++/16240
// { dg-options "-fabi-version=3 -fabi-compat-version=3" }

void foo(char);
template<void (&)(char)> struct CB {};

void g(CB<foo> i) {}

// { dg-final { scan-assembler "\n_?_Z1g2CBIL_Z3foocEE\[: \t\n\]" } }
