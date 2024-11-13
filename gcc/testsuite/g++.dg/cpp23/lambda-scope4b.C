// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++17 } }

struct integral_constant {
  using type = integral_constant;
};
template <bool> using __bool_constant = integral_constant;
template <typename _Fn, typename>
struct is_invocable : __bool_constant<true> {};
int forward() { return 42; }
template <typename...> class tuple;
struct plus {
  template <typename _Tp, typename _Up>
  constexpr auto operator()(_Tp __t, _Up __u) {
    return __t > __u;
  }
};
constexpr auto equal() {
  int t = 0;
  return [t = 3](auto obj) -> decltype(obj == t) { return t; };
}
template <typename> struct is_tuple_invocable;
template <typename... Ts> struct is_tuple_invocable<tuple<Ts...>> {
  using type = typename is_invocable<Ts...>::type;
};
namespace detail {
template <typename F, typename Tail, typename... T>
constexpr auto compose(__bool_constant<true>, F f, Tail tail, T... objs) {
  return f(tail(objs...));
}
} // namespace detail
template <typename F, typename... Fs>
constexpr auto compose(F f, Fs... fs) {
  return [f, tail(fs...)](auto... objs) -> decltype (detail::compose(typename is_tuple_invocable<tuple<decltype(objs)...>>::type{}, f, tail, objs...)) {
    auto unitail =
        typename is_tuple_invocable<tuple<decltype(objs)...>>::type{};
    return detail::compose(unitail, f, tail, objs...);
  };
}
template <auto> constexpr auto eq = equal();
static_assert(compose(eq<3>, plus{})(1, 2));
