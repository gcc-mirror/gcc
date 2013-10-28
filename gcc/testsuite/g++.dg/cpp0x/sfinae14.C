// PR c++/48557
// { dg-options -std=c++11 }

template<class T>
struct add_rval_ref
{
  typedef T&& type;
};

template<>
struct add_rval_ref<void>
{
  typedef void type;
};

template<class T>
typename add_rval_ref<T>::type create();

template<class T, class U,
  class = decltype(create<T>() + create<U>())
>
char f(int);

template<class, class>
char (&f(...))[2];

static_assert(sizeof(f<void, int>(0)) != 1, "Error");  // (a)
