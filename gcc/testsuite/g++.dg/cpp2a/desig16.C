// PR c++/95369
// { dg-do compile { target c++20 } }

struct S {
  unsigned a;
  unsigned b;
  constexpr S(unsigned _a, unsigned _b) noexcept: a{_a}, b{_b} { }
};

template<S s> struct X { };
void g(S);

struct Z {
  S s;
  Z() : s{.a = 1, .b = 2} { } // { dg-error "designated initializers|no matching function" }
};

S
f()
{
  X<{.a = 1, .b = 2}> x; // { dg-error "designated initializers" }
  S s{ .a = 1, .b = 2 }; // { dg-error "designated initializers|no matching function" }
  S s2 = { .a = 1, .b = 2 }; // { dg-error "designated initializers" }
  S s3 = S{ .a = 1, .b = 2 }; // { dg-error "designated initializers|no matching function" }
  g({.a = 1, .b = 2}); // { dg-error "designated initializers" }
  g(S{.a = 1, .b = 2}); // { dg-error "designated initializers|no matching function" }
  return {.a = 1, .b = 2}; // { dg-error "designated initializers" }
}
