// { dg-additional-options "-fmodule-header -fdump-lang-module-blocks" }
// { dg-module-cmi {} }

template<typename T> struct TPL {operator T () const {return 0;}};

template<typename T>
auto Foo (T *arg)
  -> TPL<decltype (+*arg)> {return TPL<int> ();}

template<typename T>
auto Bar (T *arg)
  -> TPL<decltype (arg)> ;

// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::Foo@[^\n]*late-ret-2_a.H:0'\n  \[1\]=specialization declaration '::TPL@[^\n]*late-ret-2_a.H:0<#null#>'\n  \[2\]=binding '::Foo'\n} module } }
