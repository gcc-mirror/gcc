// { dg-additional-options {-fmodule-header -fdump-lang-module-blocks} }

// { dg-module-cmi {} }

class frob;

template<int I> class FROB;

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::frob'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::template FROB'} module } }
