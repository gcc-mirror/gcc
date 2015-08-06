// PR c++/67130
// { dg-do compile { target c++14 } }

namespace std {
template <typename> struct __success_type;
template <typename> void declval();
template <typename> class decay {
public:
  typedef int type;
};
template <typename...> struct common_type;
struct A {
  template <typename, typename _Up>
  static __success_type<typename decay<decltype(declval<_Up>)>::type> _S_test;
};
template <typename _Tp, typename _Up> struct __common_type_impl : A {
  typedef decltype(_S_test<_Tp, _Up>) type;
};
template <typename _Tp, typename _Up>
struct common_type<_Tp, _Up> : __common_type_impl<_Tp, _Up> {};
}
template <typename> struct B { struct _; };
template <typename T> typename B<T>::_ type;
template <template <typename...> class> struct C;
template <template <typename...> class f> C<f> metafunction;
template <typename T> struct B<T>::_ {};
namespace detail {
template <typename> struct _decltype;
}
template <template <typename...> class F> struct C {
  template <typename... T>
  auto operator()(T...)
      -> decltype(type<typename F<detail::_decltype<T>...>::type>);
};
auto common_type = metafunction<std::common_type>(0, 0);
