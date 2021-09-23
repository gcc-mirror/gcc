template<typename...>
struct __and_;

template<typename _Pp>
struct __not_;

template<typename _Tp>
struct is_move_constructible;

template<typename _Tp>
struct is_nothrow_move_constructible;

template<typename _Tp>
struct is_move_assignable;

template<typename _Tp>
struct is_nothrow_move_assignable;

template<typename _Tp>
struct remove_reference;

template<bool, typename _Tp = void>
struct enable_if;

template<bool _Cond, typename _Tp = void>
using __enable_if_t = typename enable_if<_Cond, _Tp>::type;

template<typename... _Cond>
using _Require = __enable_if_t<__and_<_Cond...>::value>;

template<typename _Tp>
struct __is_tuple_like;

template<typename _Tp>
constexpr inline
  _Require<__not_<__is_tuple_like<_Tp>>,
	   is_move_constructible<_Tp>,
	   is_move_assignable<_Tp>>
swap(_Tp&, _Tp&)
  noexcept(__and_<is_nothrow_move_constructible<_Tp>,
	   is_nothrow_move_assignable<_Tp>>::value);
