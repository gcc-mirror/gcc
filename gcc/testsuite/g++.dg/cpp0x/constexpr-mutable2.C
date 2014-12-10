// PR c++/58102
// { dg-do compile { target c++11 } }

struct S {
  mutable int n;
  constexpr S() : n() {}
};

constexpr S s = {};
constexpr S s2 = s;		// { dg-error "mutable" }
