// PR c++/48319
// { dg-options -std=c++0x }
// We were failing to recognize declval<_Args1> as dependent.

template<typename Tp> Tp declval() noexcept;

template<typename _Tp>
class __is_constructible_helper
{
  typedef char __one;
  typedef struct { char __arr[2]; } __two;

  template<typename _Tp1, typename... _Args1>
  static decltype(_Tp1(declval<_Args1>()...), __one()) __test(int);

  template<typename, typename...>
  static __two __test(...);

public:
  static const bool __value = sizeof(__test<_Tp>(0)) == 1;
};

int main() {
  return __is_constructible_helper<int>::__value;
}
