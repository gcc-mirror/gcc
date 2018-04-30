// PR c++/67161
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-return-type" }

template <typename _Tp> struct integral_constant {
  static constexpr _Tp value = 0;
};
template <bool, typename, typename> struct conditional;
template <typename...> struct __or_;
template <typename _B1, typename _B2>
struct __or_<_B1, _B2> : conditional<1, _B1, _B2>::type {};
template <typename...> struct __and_;
template <typename> struct __not_ : integral_constant<bool> {};
template <typename> struct __is_void_helper : integral_constant<bool> {};
template <typename> struct is_void : __is_void_helper<int> {};
template <bool, typename _Iftrue, typename> struct conditional {
  typedef _Iftrue type;
};
template <bool _Cond, typename _Iftrue, typename _Iffalse>
using conditional_t = typename conditional<_Cond, _Iftrue, _Iffalse>::type;
template <typename...> using common_type_t = int;
template <typename, int> struct array {};
template <typename _Tp> constexpr int is_void_v = is_void<_Tp>::value;
template <typename _Dest = void, typename... _Types>
constexpr auto make_array()
    -> array<conditional_t<is_void_v<_Dest>, common_type_t<>, _Dest>,
             sizeof...(_Types)> {
  static_assert(__or_<__not_<is_void<_Dest>>, __and_<>>::value, ""); // { dg-error "static assert" }
}
auto d = make_array();
