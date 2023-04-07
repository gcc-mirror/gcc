// PR c++/108773
// { dg-do compile { target c++20 } }

#include <initializer_list>

namespace std {
template <typename _Tp, int _Nm> struct array {
  _Tp _M_elems[_Nm];
};
template <typename _Tp> struct list { void operator=(initializer_list<_Tp>); };
struct allocator {};
struct basic_string {
  int _M_p;
  constexpr basic_string() {}
  basic_string(const char *, const allocator & = allocator());
  ~basic_string();
};
} // namespace std

std::list<std::array<std::basic_string, 3>> stuff;
void foo() {
  stuff = {{"", ""}, {"", ""}};
}
