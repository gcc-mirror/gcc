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
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo@foo:.::template frob@foo:.'@foo} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s decl spec merge key \(specialization\) function_decl:'::foo@foo:.::frob<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo@foo:.::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo@foo:.::template X@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo@foo:.::X<0x0>'\n  \[1\]=specialization declaration '::foo@foo:.::X<0x0>::__conv_op <0x0>'\n  \[2\]=specialization declaration '::foo@foo:.::X<0x0>::X<0x0>'\n(  \[.\]=[^\n]* '\n)*} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s type spec merge key \(specialization\) type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Depset:. specialization entity:. type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s type spec merge key \(specialization\) type_decl:'::foo@foo:.::X<0x0>'} module } }
// { dg-final { scan-lang-dump {Wrote purview:-[0-9]* type_decl:'::foo@foo:.::X<0x0>'} module } }
