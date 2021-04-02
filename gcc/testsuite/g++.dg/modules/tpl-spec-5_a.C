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

// { dg-final { scan-lang-dump {Dependency on partial template_decl:'::template X<T,0x1>' found} module } }
// { dg-final { scan-lang-dump {Cluster members:\n(  \[.\][^\n]*'\n)*  \[.\]=partial definition '::template X<T,0x1>'} module } }
// { dg-final { scan-lang-dump {Pending specialization '::template X<T,0x1>' entity:[0-9]* section:. keyed to '::X'} module } }
