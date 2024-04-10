// PR c++/112632
// { dg-do compile { target c++20 } }

template<typename T>
inline constexpr bool C = true;

struct n {
  constexpr n(int a) : i(a) {}
  int i;
};

template<n N>
using get_n_i_type = decltype(N.i);

template<int X>
int f() {
  using iii = get_n_i_type<X>;
#if 1  // Change to 0 and this compiles
  static_assert(C<iii>);
#endif
  return iii{};
}

template int f<3>();
