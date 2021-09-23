// PR c++/102305
// { dg-do compile { target c++11 } }

namespace std
{
  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp value = __v;
      typedef integral_constant<_Tp, __v> type;
    };

  template<typename _Tp, _Tp __v>
    constexpr _Tp integral_constant<_Tp, __v>::value;

  typedef integral_constant<bool, true> true_type;
  typedef integral_constant<bool, false> false_type;

  template<bool __v>
    using bool_constant = integral_constant<bool, __v>;

  template<typename _Tp, typename... _Args>
    struct is_constructible
    : public bool_constant<__is_constructible(_Tp, _Args...)>
    {
    };
}

template<typename>
struct A {
  virtual ~A() = 0;
};

struct B {
  virtual ~B() = 0;
};

static_assert(!std::is_constructible<A<int> >::value, "");
static_assert(!std::is_constructible<B>::value, "");
