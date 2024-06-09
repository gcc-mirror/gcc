// { dg-do compile { target c++14 } }

inline auto f() {
  struct A {};
  return A{};
}
decltype(f()) a();  // { dg-error "used but not defined" "" { target c++17_down } }

auto g() {
  struct A {};
  return A{};
}
decltype(g()) b();  // { dg-error "used but never defined" }

int main() {
  a();
  b();
}
