// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
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
// { dg-final { scan-lang-dump {Writing key for mergeable specialization function_decl:'::foo::frob@bar:1<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::X@foo:2'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::X@bar:1<0x0>'\n  \[1\]=specialization declaration '::foo::X@bar:1<0x0>::__conv_op @bar:1<0x0>'\n  \[2\]=specialization declaration '::foo::X@bar:1<0x0>::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization type_decl:'::foo::X@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Depset:0 specialization type_decl:'::foo::X@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization type_decl:'::foo::X@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Voldemort:1 '::foo::X@bar:1<0x0>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:1@0 for '::foo::X@bar:1<0x0>'} module } }
