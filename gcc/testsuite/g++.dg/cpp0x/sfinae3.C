// { dg-options -std=c++0x }

namespace std { template <class T> T&& declval(); }

template<typename _Tp, typename... _Args>
  class is_constructible_mini
  {
    typedef char __one;
    typedef struct { char __arr[2]; } __two;

    template<typename _Tp1, typename... _Args1>
      static decltype(::new _Tp1(std::declval<_Args1>()...), __one())
      __test(int);

    template<typename, typename...>
      static __two __test(...);

  public:
    static const bool value = sizeof(__test<_Tp, _Args...>(0)) == 1;
  };

/*
template<typename _Tp>
  class is_constructible_mini<_Tp>
  {
    typedef char __one;
    typedef struct { char __arr[2]; } __two;

    template<typename _Tp1>
      static decltype(::new _Tp1, __one()) __test(int);

    template<typename>
      static __two __test(...);

  public:
    static const bool value
    = sizeof(__test<typename std::remove_cv<_Tp>::type>(0)) == 1;
  };
*/

struct A
{
  A(int);
};

struct B { };

static_assert( is_constructible_mini<A, int>::value, "");
static_assert( is_constructible_mini<A, A>::value, "");
static_assert( !is_constructible_mini<A, int, double>::value, "");

static_assert( !is_constructible_mini<A>::value, "");  // doesn't compile without the
                                                       // partial specialization

static_assert( is_constructible_mini<B>::value, "");
static_assert( is_constructible_mini<const B>::value, "");
