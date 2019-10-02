// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-uid-alias" }
export module bar;
// { dg-module-cmi bar }

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
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::X@foo:.::frob@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::foo::TPL'@'foo' section} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::TPL@bar:.<0x0>'\n  \[[0-9]*\]=specialization declaration '::foo::TPL@bar:.<0x0>::TPL@bar:.<0x0>'(\n  \[[0-9]*\]=clone (declaration|definition) '::foo[^'\n]* [^'\n]*')*\n  \[[0-9]*\]=decl definition '::foo::TPL@bar:.<0x0>::frob@bar:.<0x0>'} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::foo::X@foo:.::frob@bar:.<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing type spec key for mergeable specialization type_decl:'::foo::TPL@bar:.<0x0>'} module } }
// { dg-final { scan-lang-dump {Writing decl spec key for mergeable specialization function_decl:'::foo::X@foo:.::frob@bar:.<0x0>'} module } }
