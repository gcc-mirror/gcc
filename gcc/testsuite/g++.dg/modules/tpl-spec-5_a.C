// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module TPL;
// { dg-module-cmi TPL }

export template <typename T, int I>
struct X
{
  T ary[I];
};

template<typename T> struct X<T,1>
{
  T scalar;
};

// { dg-final { scan-lang-dump {Dependency on specialization template_decl:'::X<T,0x1>' added} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization definition '::X<T,0x1>'} module } }
// { dg-final { scan-lang-dump {Specialization '::X<T,0x1>' entity:[0-9]* keyed to '::X' \(2\)} module } }
