// { dg-additional-options "-fmodule-header -fdump-lang-module-alias" }
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

// { dg-final { scan-lang-dump {Ordering 4 depsets\n  Mergeable 0 is '::Foo@[^\n]*/late-ret-3_a.H:1'\n  Mergeable 1 is '::TPL_1@[^\n]*/late-ret-3_a.H:1<#null#>' \(tight\)\n  Mergeable 2 is '::TPL_3@[^\n]*/late-ret-3_a.H:1<::TPL_1@[^\n]*/late-ret-3_a.H:1<#null#>::type@[^\n]*>' \(tight\)\n  Mergeable 3 is '::TPL_2@[^\n]*/late-ret-3_a.H:1<::TPL_1@[^\n]*/late-ret-3_a.H:1<#null#>::type@[^\n]*>'\n} module } }
