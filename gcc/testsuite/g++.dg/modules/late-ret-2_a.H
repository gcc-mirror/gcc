// { dg-additional-options "-fmodule-header -fdump-lang-module-alias" }
// { dg-module-cmi {} }

template<typename T> struct TPL {operator T () const {return 0;}};

template<typename T>
auto Foo (T *arg)
  -> TPL<decltype (+*arg)> {return TPL<int> ();}

template<typename T>
auto Bar (T *arg)
  -> TPL<decltype (arg)> ;

// { dg-final { scan-lang-dump {  Ordering 2 depsets\n  Mergeable 1 is '::Foo@[^\n]*/late-ret-2_a.H:.'\n  Mergeable 2 is '::TPL@[^\n]*/late-ret-2_a.H:.<#null#>' \(tight\)\n  Ordered 2 mergeables} module } }
