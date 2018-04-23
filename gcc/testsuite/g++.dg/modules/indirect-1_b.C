// { dg-additional-options -fdump-lang-module }
export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar 
{
  export int frob (int i = foo::frob (0))
  {
    return i;
  }
}

// { dg-final { scan-lang-dump {Lazily loading '::foo::frob'@'foo' section:} "module" } }
// { dg-final { scan-lang-dump {Wrote named import:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Wrote named import:-[0-9]* function_decl:'::foo::frob'@foo} module } }
