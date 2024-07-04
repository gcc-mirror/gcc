// { dg-additional-options "-fmodules-ts -Wno-error=c++20-extensions" }
// { dg-module-cmi M }

export module M;

auto f() {
  struct A {};
  return A{};
}
decltype(f()) g();  // { dg-warning "used but not defined" "" { target c++17_down } }
export auto x = g();
