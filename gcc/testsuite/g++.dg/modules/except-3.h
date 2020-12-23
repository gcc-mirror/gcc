
template<typename _Tp>
struct is_nothrow_move_constructible
{
  static constexpr bool value = false;
};

template<typename _Head>
struct _Tuple_impl
{
  _Tuple_impl () noexcept(is_nothrow_move_constructible<_Head>::value)
 { }
};

template<typename T>
void TPL (_Tuple_impl<T> &) noexcept
{
  _Tuple_impl<T> m;
}

inline void foo (_Tuple_impl<int> &p)
{
  TPL<int> (p);
}
