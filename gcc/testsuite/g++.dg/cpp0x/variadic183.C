// PR c++/105956
// { dg-do compile { target c++11 } }

template<int...> struct list;

template<int... Ns> struct impl {
  static const int idx = 0;
  using type = list<(idx + Ns)...>;

  static constexpr const int* a[2] = {(Ns, &idx)...};
  static_assert(a[0] == &idx && a[1] == &idx, "");
};

template struct impl<0, 1>;
