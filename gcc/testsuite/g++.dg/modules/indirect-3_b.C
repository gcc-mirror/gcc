// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-uid-alias" }
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

// { dg-final { scan-lang-dump {Lazily binding '::foo::TPL'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL@foo:2'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x0>'\n  \[1\]=specialization declaration '::foo::TPL@bar:1<0x0>::TPL<0x0>'\n  \[2\]=decl definition '::foo::TPL@bar:1<0x0>::frob@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::X@foo:2::frob@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization type_decl:'::foo::TPL@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization function_decl:'::foo::X@foo:2::frob@bar:1<0x0>'} module } }
