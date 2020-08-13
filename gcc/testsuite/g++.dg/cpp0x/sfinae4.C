// { dg-do compile { target c++11 } }

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

// int[](...) will work with P0960 and P1009.
#if __cpp_aggregate_paren_init
constexpr bool r = true;
#else
constexpr bool r = false;
#endif
static_assert( is_constructible_mini<int[], int>::value == r, "");
static_assert( !is_constructible_mini<void, int>::value, "");
