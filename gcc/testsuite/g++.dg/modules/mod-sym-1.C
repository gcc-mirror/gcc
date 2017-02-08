module linkage [[interface]];
// { dg-module-if "linkage" }

inline void Foo () {}

export inline void Baz () __attribute__((used));
export inline void Baz () { Foo (); }

extern "C" inline void cfunc (void) __attribute__((used));
extern "C" inline void cfunc (void) {}

namespace Inner
{
  inline void Foo2 () {}

  export inline void Baz2 () __attribute__((used));
  export inline void Baz2 () { Foo2 (); }
}

// { dg-final { scan-assembler "_ZN9_Mlinkage3FooEv:" } }
// { dg-final { scan-assembler "_ZN5Inner9_Mlinkage4Foo2Ev:" } }
// { dg-final { scan-assembler "_Z3Bazv:" } }
// { dg-final { scan-assembler "_ZN5Inner4Baz2Ev:" } }
// { dg-final { scan-assembler "cfunc:" } }
