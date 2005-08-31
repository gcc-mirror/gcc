// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR101: Redeclaration of extern "C" names via using-declarations 

namespace Test1 {

  typedef unsigned int X;
  extern "C" void f1();
  namespace N {
    typedef unsigned int X;
    extern "C" void f1();
  }
  using N::f1; // { dg-bogus "" "redeclaration through 'using' should not be ambiguous" }
  using N::X;  // { dg-bogus "" "redeclaration through 'using' should not be ambiguous" }
}


namespace Test2 {

  typedef unsigned int X;   // { dg-bogus "declared" "" { xfail *-*-* } }
  extern "C" int f2();
  namespace N {
    typedef unsigned int X; // { dg-bogus "declared" "" { xfail *-*-* } }
    extern "C" int f2();
  }
  using namespace N;
  int i = f2(); // { dg-bogus "" "redeclaration through 'using' should not be ambiguous" }
  X x;          // { dg-bogus "" "redeclaration through 'using' should not be ambiguous" { xfail *-*-* } }

}

