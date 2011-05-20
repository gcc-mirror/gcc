// PR c++/48745
// { dg-options -std=c++0x }

template<class T>
struct add_rval_ref {
  typedef T&& type;
};

template<>
struct add_rval_ref<void> {
  typedef void type;
};

template<class T>
typename add_rval_ref<T>::type create();

template<class T, class... Args>
decltype(T{create<Args>()...}, char()) f(int);

template<class, class...>
char (&f(...))[2];

static_assert(sizeof(f<int, void>(0)) != 1, "Error"); // { dg-bogus "void value" "" { xfail *-*-* } }
