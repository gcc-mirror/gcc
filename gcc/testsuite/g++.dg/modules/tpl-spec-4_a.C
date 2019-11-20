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

// { dg-final { scan-lang-dump {Dependencies of specialization type_decl:'::X@TPL:.<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::X@TPL:.<int>'} module } }
// { dg-final { scan-lang-dump {Specialization '::X@TPL:.<int>' entity:[0-9]* keyed to '::X@TPL:.<T>' \(2\)} module } }
