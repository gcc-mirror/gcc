
struct __do_is_destructible_impl
{
  template<typename _Tp, typename = decltype(_Tp().~_Tp())>
  static bool __test(int);

  template<typename>
  static float __test(...);
};

template<typename _Tp>
struct __is_destructible_impl
  : public __do_is_destructible_impl
{
  // Requires BINFO merging
  typedef decltype(__test<_Tp>(0)) type;
};
