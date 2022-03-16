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

// { dg-final { scan-assembler "_ZW7linkage3Foov:" } }
// { dg-final { scan-assembler "_ZW7linkage4Binkv:" } }
// { dg-final { scan-assembler "_ZN5InnerW7linkage4Foo2Ev:" } }
// { dg-final { scan-assembler "_ZN5InnerW7linkage5Bink2Ev:" } }
// { dg-final { scan-assembler "_ZW7linkage3Bazv:" } }
// { dg-final { scan-assembler "_ZN5InnerW7linkage4Baz2Ev:" } }
// { dg-final { scan-assembler "cfunc:" } }
