// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module TPL;
// { dg-module-bmi TPL }

export template <typename T, int I>
struct X
{
  T ary[I];
};

template<typename T> struct X<T,1>
{
  T scalar;
};

// { dg-final { scan-lang-dump {Dependency on specialization template_decl:'::X@TPL:1<T,0x1>' added} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::X@TPL:1<T,0x1>'} module } }
// { dg-final { scan-lang-dump {Specialization '::X@TPL:1<T,0x1>' section:2 keyed to '::X@TPL:1<T,#unnamed#>' \(2\)} module } }
