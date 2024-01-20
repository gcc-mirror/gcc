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
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::template TPL@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL<0x1>'\n  \[1\]=specialization declaration '::foo::TPL<0x1>::template frob<#unnamed#>'\n  \[2\]=specialization declaration '::foo::TPL<0x1>::TPL<0x1>'\n(  \[.\]=[^\n]* '\n)*} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL<0x1>::frob<0x2>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s type spec merge key \(specialization\) type_decl:'::foo::TPL<0x1>'} module } }
// { dg-final { scan-lang-dump {Wrote purview:-[0-9]* type_decl:'::foo::TPL<0x1>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s decl spec merge key \(specialization\) function_decl:'::foo::TPL<0x1>::frob<0x2>'} module } }
