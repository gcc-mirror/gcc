// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module TPL;
// { dg-module-bmi TPL }

export template <typename T>
struct X
{
  T f;
};

template<> struct X<int>
{
  int m;
};

// { dg-final { scan-lang-dump {Dependencies of specialization type_decl:'::X@TPL:1<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::X@TPL:1<int>'} module } }
// { dg-final { scan-lang-dump {Specialization '::X@TPL:1<int>' section:2 keyed to '::X@TPL:1<T>' \(2\)} module } }
