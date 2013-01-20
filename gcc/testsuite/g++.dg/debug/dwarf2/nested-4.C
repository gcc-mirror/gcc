// PR debug/53235
// { dg-options "-gdwarf-4 -fdebug-types-section" }
// { dg-final { scan-assembler-times "debug_types" 2 { xfail *-*-darwin* } } }

namespace E {
  class O {};
  void f (O o) {}
}
namespace F {
  class O {};
  void f (O fo) {}
}
E::O eo;
int main () {}
