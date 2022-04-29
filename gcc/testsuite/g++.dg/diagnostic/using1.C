// PR c++/102987
// { dg-do compile { target c++11 } }

struct a {
  bool b();
};
template <typename c> struct d : c {
  using c::e;
  using f = d;
  // { dg-message "decltype .c::e" "" { target *-*-* } 0 }
  constexpr int g(decltype(e.b())) { return buh; } // { dg-error "buh" }
};
struct h {
  a e;
};
using i = d<h>;
auto j = i{}.g(1);
