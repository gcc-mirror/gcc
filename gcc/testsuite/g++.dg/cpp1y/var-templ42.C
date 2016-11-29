// PR c++/67131
// { dg-do compile { target c++14 } }

template <typename T> typename T::_ type;
template <template <typename...> class> struct A;
template <template <typename> class f> A<f> metafunction;
namespace detail {
template <typename> struct _decltype;
}
template <template <typename...> class F> struct A {
  template <typename... T>
  auto operator()() -> decltype(type<F<detail::_decltype<T>...>>);
};
template <typename F> auto valid_call(F f) -> decltype(f());
constexpr auto valid_call(...) { return 0; }
template <typename> struct no_type;
static_assert(!valid_call(metafunction<no_type>),"");
