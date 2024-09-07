// PR c++/106761

template<class...>
struct __and_;

template<class, class>
struct is_convertible;

template<class... Ts>
struct _TupleConstraints {
  template<class... Us>
  using __constructible = __and_<is_convertible<Ts, Us>...>;
};

template<class... Ts>
struct tuple {
  template<class... Us>
  using __constructible
    = typename _TupleConstraints<Ts...>::template __constructible<Us...>;
};

inline tuple<int, int> t;
