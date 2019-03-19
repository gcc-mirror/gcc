// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

export module bob;
// { dg-module-bmi bob }

namespace N {
export int foo ();
}

// Only one using decl exported!
using N::foo;
export using N::foo;

// { dg-final { scan-lang-dump {Writing SCC:2 2 depsets} module } }
// { dg-final { scan-lang-dump {Depset:0 using function_decl:'::N::foo'} module } }
// { dg-final { scan-lang-dump {Depset:1 binding namespace_decl:'::foo'} module } }

