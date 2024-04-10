// PR c++/111607
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

#include <variant>

struct S {
	constexpr S(int i_) : i(i_) {}
	S(S const &) = delete;
	S & operator=(S const &) = delete;
	S(S &&) = delete;
	S & operator=(S &&) = delete;
	int i;
};

struct A {
	S s{0};
};

using V = std::variant<A>;

consteval auto f(V const & v) {
  auto const & s = std::visit([](auto const & v) -> S const & { return v.s; }, v); // { dg-bogus "dangling reference" }
  return s.i;
}

int main() {
	constexpr V a{std::in_place_type<A>};
	constexpr auto i = f(a);
	return i;
}
