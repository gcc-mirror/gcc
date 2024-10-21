// { dg-additional-options "-fmodule-header -fdump-lang-module" }
// { dg-module-cmi {} }

struct A { int a; };
template <typename T> struct B { int b; };

// { dg-final { scan-lang-dump {Pendings 2} module } }
