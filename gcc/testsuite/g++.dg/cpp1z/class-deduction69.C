// PR c++/90764
// { dg-do compile { target c++17 } }

namespace a {
  struct b;
  template <typename...> using c = b;
}
template <typename... d> struct e : a::c<d...> { // { dg-error "incomplete" }
  using a::c<>::c;		// { dg-prune-output "not a direct base" }
};
template <template <typename> typename f> void g() { f(); }
void h() { g<e>(); }
