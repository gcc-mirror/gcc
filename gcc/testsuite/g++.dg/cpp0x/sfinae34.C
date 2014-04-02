// PR c++/52422
// { dg-do compile { target c++11 } }

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
  class = decltype( (create<T>().*create<U>())() )
>
auto f(int) -> char(&)[1];

template<class, class>
auto f(...) -> char(&)[2];

static_assert(sizeof(f<void, void>(0)) != 1, "");
