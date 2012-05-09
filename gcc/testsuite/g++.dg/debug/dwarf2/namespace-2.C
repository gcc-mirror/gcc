// Test that we define A inside the namespace rather than declaring it
// there and then defining it at CU scope.
// { dg-options "-g -dA" }
// { dg-final { scan-assembler-not "DW_AT_declaration" } }

namespace N {
  struct A;
}

struct N::A { } a;
