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

// { dg-final { scan-lang-dump {Cluster:1 3 depsets\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x0>'\n  \[1\]=specialization definition '::foo::TPL@bar:1<0x0>::frob@bar:1<0x0>'\n  \[2\]=specialization declaration '::foo::TPL@bar:1<0x0>::TPL<0x0>'} module } }
// { dg-final { scan-lang-dump {Cluster:3 1 depsets\n  \[0\]=specialization definition '::foo::X@foo:2::frob@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global specialization type_decl:'::foo::TPL@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global specialization function_decl:'::foo::X@foo:2::frob@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Voldemort:3 '::foo::X@foo:2::frob@bar:1<0x0>'} module } }
