// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename T> struct TPL_1 { using type = T;};

template<typename U> struct TPL_2 { using type = int;};

template<typename V> using TPL_3 = typename TPL_2<V>::type;

template<typename A>
auto Foo (const A& arg)
  -> TPL_3<typename TPL_1<decltype(arg)>::type>
  {return 3;}

// Deliberately different to 3_a's Bar
template<typename A>
auto Bar (const A& arg)
  -> TPL_3<typename TPL_1<int>::type>
  {return 3;}

