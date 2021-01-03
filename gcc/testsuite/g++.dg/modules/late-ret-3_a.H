// { dg-additional-options "-fmodule-header -fdump-lang-module-blocks" }
// { dg-module-cmi {} }

template<typename T> struct TPL_1 { using type = T;};

template<typename U> struct TPL_2 { using type = int;};

template<typename V> using TPL_3 = typename TPL_2<V>::type;

template<typename A>
auto Foo (const A& arg)
  -> TPL_3<typename TPL_1<decltype(arg)>::type>
  {return 3;}

template<typename A>
auto Bar (const A& arg)
  -> TPL_3<typename TPL_1<decltype(arg)>::type>
  {return 3;}

// { dg-final { scan-lang-dump { Cluster members:\n  \[0\]=decl definition '::template Foo'\n  \[1\]=specialization declaration '::TPL_1<#null#>'\n  \[2\]=specialization declaration '::TPL_3<::TPL_1<#null#>::type>'\n  \[3\]=specialization declaration '::TPL_2<::TPL_1<#null#>::type>'\n  \[4\]=binding '::Foo'\n} module } }
