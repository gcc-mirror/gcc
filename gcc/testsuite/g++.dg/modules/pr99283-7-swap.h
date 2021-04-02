template<typename _Tp>
constexpr typename remove_reference<_Tp>::type&&
  move(_Tp&& __t) noexcept;

template<typename _Tp>
constexpr inline
typename enable_if<__and_<__not_<__is_tuple_like<_Tp>>,
			  is_move_constructible<_Tp>,
			  is_move_assignable<_Tp>>::value>::type
  swap(_Tp& __a, _Tp& __b)
  noexcept(__and_<is_nothrow_move_constructible<_Tp>,
	   is_nothrow_move_assignable<_Tp>>::value)
{
  _Tp __tmp = move(__a);
  __a = move(__b);
  __b = move(__tmp);
}
