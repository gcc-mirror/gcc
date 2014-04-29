// PR c++/51707
// { dg-do compile { target c++11 } }

struct S {
	constexpr S() {}
};

struct T {
	constexpr T(S const& s) : s{s} {}
	S const& s;
};

constexpr S s {};
constexpr T t { s };
