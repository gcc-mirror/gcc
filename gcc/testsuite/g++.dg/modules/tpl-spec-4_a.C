// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module TPL;
// { dg-module-cmi TPL }

export template <typename T>
struct X
{
  T f;
};

template<> struct X<int>
{
  int m;
};

// { dg-final { scan-lang-dump {Dependencies of specialization type_decl:'::X<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n(  \[.\]=[^\n]*'\n)*  \[.\]=specialization definition '::X<int>'} module } }
// { dg-final { scan-lang-dump {Pending specialization '::X<int>' entity:[0-9]* section:. keyed to '::X'} module } }
