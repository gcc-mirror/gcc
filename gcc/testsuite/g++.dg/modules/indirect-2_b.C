// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
export module bar;
// { dg-module-cmi bar }

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

// { dg-final { scan-lang-dump {Lazily binding '::foo@foo:.::frob'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo@foo:.::frob@foo:.'@foo} module } }
// { dg-final { scan-lang-dump {Writing decl spec key for mergeable specialization function_decl:'::foo@foo:.::frob<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo@foo:.::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo@foo:.::X@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo@foo:.::X<0x0>'\n  \[1\]=specialization declaration '::foo@foo:.::X<0x0>::X<0x0>'\n  \[2\]=specialization declaration '::foo@foo:.::X<0x0>::__conv_op <0x0>'\n} module } }
// { dg-final { scan-lang-dump {Writing type spec key for mergeable specialization type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Depset:0 specialization entity:. type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing type spec key for mergeable specialization type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Wrote purview:-[0-9]* type_decl:'::foo@foo:.::X<0x0>'} module } }
