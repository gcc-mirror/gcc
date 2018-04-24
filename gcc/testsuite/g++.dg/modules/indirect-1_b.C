// { dg-additional-options -fdump-lang-module }
export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar 
{
  export int frob (int i = foo::frob (0))
  {
    return i;
  }

  export int quux (int i = foo::X (0) )
  {
    return i;
  }

  export class Z : public foo::Y
  {
  public:
    Z (int i, int j) : X(i), Y(i, j)
    {
    }
  };

  export constexpr auto Plain_One (bool b) { return b ? foo::B : foo::C; }
  export constexpr auto Scoped_One (bool b) { return b ? foo::Scoped::B
      : foo::Scoped::C; }

  export extern auto const Plain_Const_Three = foo::D;
  export extern auto const Scoped_Const_Three = foo::Scoped::D;
}

// { dg-final { scan-lang-dump {Lazily loading '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* function_decl:'::foo::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::foo::X'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::foo::Y'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::foo::Y'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::foo::B'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily loading '::foo::C'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Lazily loading '::foo::Scoped'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily loading '::foo::Scoped::[ABCD]'@'foo' section:} module } }

// { dg_final { scan-lang-dump {Wrote named import:-[0-9]* const_decl:'::foo::Plain::C'@foo} module } }
// { dg_final { scan-lang-dump {Wrote named import:-[0-9]* const_decl:'::foo::Plain::B'@foo} module } }
// { dg_final { scan-lang-dump {Wrote named import:-[0-9]* const_decl:'::foo::Scoped::C'@foo} module } }
// { dg_final { scan-lang-dump {Wrote named import:-[0-9]* const_decl:'::foo::Scoped::B'@foo} module } }
