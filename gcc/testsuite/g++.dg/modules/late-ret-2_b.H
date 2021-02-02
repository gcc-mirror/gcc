// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename T> struct TPL {operator T () const {return 0;}};

template<typename T>
auto Foo (T *arg)
  -> TPL<decltype (+*arg)> {return TPL<int> ();}

// Deliberately different to 2_a's Bar
template<typename T>
auto Bar (T *arg)
  -> TPL<decltype (*arg)> ;
