// { dg-do assemble { xfail *-*-* } }

// Submitted by bjornw@fairplay.no


namespace hei {
  class CSomeClass {};
  extern CSomeClass SomeClass;
}

hei::CSomeClass hei::CSomeClass; // { dg-error "" "" { xfail *-*-* } } should be hei::SomeClass - 
