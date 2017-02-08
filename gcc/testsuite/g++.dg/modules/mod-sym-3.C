module bob [[interface]];
// { dg-module-if "bob" }

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

// { dg-final { scan-assembler "_ZN1X5_Mbob3FooEv:" } }
// { dg-final { scan-assembler "_ZN1X3BazEv:" } }
// { dg-final { scan-assembler "_ZN1X1Y5_Mbob4QuuxEv:" } }
// { dg-final { scan-assembler "_ZN1X1Y3BarEv:" } }
