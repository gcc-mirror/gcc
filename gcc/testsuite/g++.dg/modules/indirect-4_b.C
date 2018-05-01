// { dg-additional-options -fdump-lang-module }
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

// { dg-final { scan-lang-dump {Lazily loading '::foo::TPL'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL'@foo} module } }
// { dg-final { scan-lang-dump {Wrote instantiation:-[0-9]* type_decl:'::foo::TPL'@} module } }
// { dg-final { scan-lang-dump {Wrote import:-[0-9]* template_decl:'::foo::TPL::frob'@foo} module } }
// { dg-final { scan-lang-dump {Wrote instantiation:-[0-9]* function_decl:'::foo::TPL::frob'@} module } }
