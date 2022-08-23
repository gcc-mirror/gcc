// { dg-additional-options "-fmodules-ts" }
export module bob;
// { dg-module-cmi "bob" }

namespace X
{
  inline void Foo () __attribute__((used));
  export inline void Baz () __attribute__((used));

  namespace Y
  {
    inline void Quux () __attribute__((used));
    export inline void Bar () __attribute__((used));
  }

  inline void Y::Quux () {}
  inline void Y::Bar () {}
}

inline void X::Foo () {}
inline void X::Baz () {}

// { dg-final { scan-assembler "_ZN1XW3bob3FooEv:" } }
// { dg-final { scan-assembler "_ZN1XW3bob3BazEv:" } }
// { dg-final { scan-assembler "_ZN1X1YW3bob4QuuxEv:" } }
// { dg-final { scan-assembler "_ZN1X1YW3bob3BarEv:" } }
