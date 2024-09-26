// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

export module bar;
// { dg-module-cmi bar }

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

// { dg-final { scan-lang-dump {Lazily binding '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {namespace:-[0-9]* namespace_decl:'::foo'} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* function_decl:'::foo::frob@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::foo::X@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::Y'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* type_decl:'::foo::Y@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::B'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::C@foo:.'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::foo::Scoped'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::Scoped@foo:.::[ABCD]'@'foo' section:} module } }

// { dg-final { scan-lang-dump-times {Wrote import:-[0-9]* type_decl:'::foo::Plain@foo:.'@foo} 2 module } }
// { dg-final { scan-lang-dump-times {Wrote import:-[0-9]* type_decl:'::foo::Scoped@foo:.'@foo} 2 module } }
