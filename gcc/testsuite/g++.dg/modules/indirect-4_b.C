// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar 
{
  export int quux (int i = foo::TPL<1> ().frob<2> ())
  {
    return i;
  }
}

// { dg-final { scan-lang-dump {Lazily binding '::foo::TPL'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL@foo:2'@foo} module } }

// { dg-final { scan-lang-dump {Cluster:1 3 depsets\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x1>'\n  \[1\]=specialization declaration '::foo::TPL@bar:1<0x1>::TPL<0x1>'\n  \[2\]=specialization declaration '::foo::TPL@bar:1<0x1>::frob<#unnamed#>'} module } }
// { dg-final { scan-lang-dump {Cluster:2 1 depsets\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x1>::frob@bar:1<0x2>'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global specialization type_decl:'::foo::TPL@bar:1<0x1>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::foo::TPL@bar:1<0x1>'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global specialization function_decl:'::foo::TPL@bar:1<0x1>::frob@bar:1<0x2>'} module } }
