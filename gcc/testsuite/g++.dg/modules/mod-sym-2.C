inline void Foo () {}

module okely.dokely [[interface]];
// { dg-module-if "okely.dokely" }

namespace One {
  namespace Two {
    inline namespace Three
    {
      inline void Foo2 () {}
      
      export inline void Baz2 () __attribute__((used));
      export inline void Baz2 () { Foo (); Foo2 (); }
    }
  }
}

// These fail until namespace hack is removed
// { dg-final { scan-assembler "_Z3Foov:" } }
// { dg-final { scan-assembler "_ZW5okely6dokelyEN3One3Two5Three4Foo2Ev:" { xfail *-*-* } } }
// { dg-final { scan-assembler "_ZN3One3Two5Three4Baz2Ev:" } }
