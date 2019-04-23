// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar
{
  export int quux (int i = foo::X().frob<0> ())
  {
    return i;
  }

  export int toto (int i = foo::TPL<0>().frob ())
  {
    return i;
  }
}

// { dg-final { scan-lang-dump {Lazily binding '::foo::X'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::X@foo:2::frob'@foo} module } }
// { dg-final { scan-lang-dump {Wrote instantiation:-[0-9]* function_decl:'::foo::X@foo:2::frob@bar:1<0x0>'@} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::TPL'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Wrote instantiation:-[0-9]* type_decl:'::foo::TPL@foo:2<0x0>'@} module } }
// { dg-final { scan-lang-dump {Wrote member:-[0-9]* function_decl:'::foo::TPL@foo:2<0x0>::frob<0x0>'@bar} module } }
