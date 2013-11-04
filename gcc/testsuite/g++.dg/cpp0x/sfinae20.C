// PR c++/48744
// { dg-options "-std=c++11" }

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

template<class T, class Arg>
decltype(T{create<Arg>()}, char()) f(int);

template<class, class>
char (&f(...))[2];

static_assert(sizeof(f<int, void>(0)) != 1, "Error");
