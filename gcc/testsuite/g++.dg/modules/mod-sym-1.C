// { dg-additional-options "-fmodules-ts" }
export module linkage;
// { dg-module-cmi "linkage" }

inline void Foo () {}

export inline void Baz () __attribute__((used));

inline void Bink () {}

export inline void Baz () { Foo (); Bink (); }

extern "C" inline void cfunc (void) __attribute__((used));
extern "C" inline void cfunc (void) {}

namespace Inner
{
  inline void Foo2 () {}

  export inline void Baz2 () __attribute__((used));

  inline void Bink2 () {}
  
  export inline void Baz2 () { Foo2 (); Bink2 (); }
}

// These fail until namespace hack is removed
// { dg-final { scan-assembler "_ZW7linkageE3Foov:" } }
// { dg-final { scan-assembler "_ZW7linkageE4Binkv:" } }
// { dg-final { scan-assembler "_ZW7linkageEN5Inner4Foo2Ev:" } }
// { dg-final { scan-assembler "_ZW7linkageEN5Inner5Bink2Ev:" } }
// { dg-final { scan-assembler "_Z3Bazv:" } }
// { dg-final { scan-assembler "_ZN5Inner4Baz2Ev:" } }
// { dg-final { scan-assembler "cfunc:" } }
