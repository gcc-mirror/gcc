// PR c++/85214
// { dg-do compile { target c++17 } }

struct g {
  constexpr operator int() { return true; }
};
template <typename T, typename U> constexpr bool m = true;
template <long L> struct C { typedef double q; };
void ao() {
  [](auto i) {
      using ar = typename C<i>::q;
      [](auto j) {
	using as = typename C<j>::q;
	if constexpr (m<ar, as>) {}
	// { dg-bogus "'i' is not captured" "PR107437" { xfail { *-*-* } } .-1 }
      }(g());
  }(g());
}
