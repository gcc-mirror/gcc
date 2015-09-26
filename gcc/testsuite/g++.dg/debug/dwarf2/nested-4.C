// PR debug/53235
// { dg-options "-gdwarf-4 -fdebug-types-section" }
// { dg-final { scan-assembler-times "debug_types" 2 { xfail { powerpc-ibm-aix* || { *-*-darwin* || { *-*-solaris2.1[1-9]* && { ! gas } } } } } } }

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
