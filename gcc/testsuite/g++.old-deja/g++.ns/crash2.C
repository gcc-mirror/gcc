// Build don't link:

// Submitted by bjornw@fairplay.no

// crash test - XFAIL *-*-*

namespace hei {
  class CSomeClass {};
  extern CSomeClass SomeClass;
};

hei::CSomeClass hei::CSomeClass; // ERROR - should be hei::SomeClass
