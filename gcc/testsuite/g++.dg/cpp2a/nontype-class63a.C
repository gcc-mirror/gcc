// PR c++/112594
// { dg-do compile { target c++20 } }

template<typename T>
concept C = true;

struct n {
  constexpr n(int) {}
  static int i;
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
