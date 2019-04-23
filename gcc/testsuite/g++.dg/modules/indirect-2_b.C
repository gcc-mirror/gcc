// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar
{
  export int frob (int i = foo::frob<0> ())
  {
    return i;
  }

  export int quux (int i = foo::X<0> ())
  {
    return i;
  }
}

// { dg-final { scan-lang-dump {Lazily binding '::foo::frob'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::frob@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Wrote mergeable:-[0-9]* function_decl:'::foo::frob@bar:1<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::X@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Wrote instantiation:-[0-9]* type_decl:'::foo::X@foo:2<0x0>'@} module } }

