// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-require-effective-target lto }
// { dg-options "-flto" }

typedef long unsigned int size_t;
struct basic_string_view {
  typedef long unsigned int size_type;
  constexpr size_type size() const { return 0; }
};
struct array {
  char _M_elems[1];
};
inline constexpr auto make_it() {
  constexpr basic_string_view view;
  array arr{};
  arr._M_elems[view.size()] = 'a';
  return arr;
}
auto bar = make_it();
