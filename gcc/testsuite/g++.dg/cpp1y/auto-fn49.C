// CWG issue 2335
// { dg-do compile { target c++14 } }

template <class... Ts> struct partition_indices {
  static auto compute_right () {}
  static constexpr auto right = compute_right;
};
auto foo () -> partition_indices<>;
void f() {
  auto x = foo();
  auto y = x.right;
}
