// PR c++/120684
// { dg-do compile { target c++20 } }

struct basic_string {
  constexpr ~basic_string() {}
};
template <typename _Vp> struct lazy_split_view {
  _Vp _M_base;
  constexpr int* begin() { return nullptr; }
  constexpr int* end() { return nullptr; }
};
constexpr void test_with_piping() {
  basic_string input;
  for (auto e : lazy_split_view(input))
    ;
}
constexpr bool main_test() {
  test_with_piping();
  test_with_piping();
  return true;
}
//int main() { main_test(); }
static_assert(main_test());
