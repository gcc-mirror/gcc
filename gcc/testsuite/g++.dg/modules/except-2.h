
// Causes the CMI to have instantiated a deferred noexept spec that
// the textually included file has not.

typedef long unsigned int size_t;


template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
  typedef integral_constant<_Tp, __v> type;
};
template<typename _Tp, _Tp __v>
constexpr _Tp integral_constant<_Tp, __v>::value;

template<typename _Head>
struct _Tuple_impl : _Head
{
  _Tuple_impl(_Tuple_impl&& __in)
    noexcept (integral_constant<bool,
	      noexcept(_Head(static_cast<_Head &&>(*(_Head *) (0))))>::type::value);
};

template <typename _Dp>
struct __uniq_ptr_impl
{
  __uniq_ptr_impl (__uniq_ptr_impl&& __u) noexcept
    : _M_t(static_cast <_Tuple_impl<_Dp> &&>(__u._M_t))
  {}

  _Tuple_impl<_Dp> _M_t;
};

struct _Impl_deleter {};

typedef __uniq_ptr_impl<_Impl_deleter> up;

inline void frob (up && p)
{
  up _M_cmpts (static_cast <up &&>  (p));
}
