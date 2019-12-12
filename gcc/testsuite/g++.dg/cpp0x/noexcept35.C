// { dg-do compile { target c++11 } }

template <typename _Tp, _Tp __v> struct A { static constexpr _Tp value = __v; };
typedef A<bool, false> false_type;
struct is_same : false_type {};
template <bool> struct enable_if;
template <typename> using __remove_cvref_t = int;
template <typename _Tp> class reference_wrapper {
  static _Tp _S_fun();
  template <typename _Up, typename = __remove_cvref_t<_Up>>
  using __not_same = enable_if<is_same::value>;

public:
  template <typename _Up, typename = __not_same<_Up>>
  reference_wrapper(_Up) noexcept(noexcept(reference_wrapper::_S_fun));
};

reference_wrapper<int> fn1() {
  int __t = 10;
  return __t;
}
