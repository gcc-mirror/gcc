// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
export module bar;
// { dg-module-cmi bar }

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

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x1>'\n(  \[.\]=clone (declaration|definition) '::hidey[^\n]* '\n)*  \[.*\]=specialization declaration '::foo::TPL@bar:1<0x1>::frob<#unnamed#>'\n  \[.*\]=specialization declaration '::foo::TPL@bar:1<0x1>::TPL<0x1>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL@bar:1<0x1>::frob@bar:1<0x2>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization type_decl:'::foo::TPL@bar:1<0x1>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::foo::TPL@bar:1<0x1>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization function_decl:'::foo::TPL@bar:1<0x1>::frob@bar:1<0x2>'} module } }
