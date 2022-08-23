// PR c++/104008
// { dg-do compile { target c++11 } }

template <typename...> struct conjunction;
template <typename...> struct disjunction;
template <typename, typename> struct is_same;
template <bool> struct enable_if;
template <bool _Cond> using enable_if_t = typename enable_if<_Cond>::type;
struct B;
struct __uniq_ptr_impl {
  struct _Ptr {
    using type = B *;
  };
  using pointer = _Ptr::type;
};
struct unique_ptr {
  using pointer = __uniq_ptr_impl::pointer;
  unique_ptr(pointer);
};
template <typename T, typename... Ts>
using IsOneOf = disjunction<is_same<T, Ts>...>;

template <typename...> struct any_badge;

struct badge {
  badge(any_badge<>);
  badge();
};

template <typename...> struct any_badge {
  template <typename... OtherHolders,
            enable_if_t<conjunction<IsOneOf<OtherHolders>...>::value>>
  any_badge();
};

template <typename, typename... _Args> unique_ptr make_unique(_Args... __args);

struct B {
  B(badge);
  unique_ptr b_ = make_unique<B>(badge{});
};

template <typename, typename... _Args> unique_ptr make_unique(_Args... __args) {
  return new B(__args...);
}
