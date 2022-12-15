// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++23 } }
// { dg-options "-Winvalid-constexpr" }
// A copy/move assignment operator for a class X that is defaulted and
// not defined as deleted is implicitly defined when it is odr-used,
// when it is needed for constant evaluation, or when it is explicitly
// defaulted after its first declaration.
// The implicitly-defined copy/move assignment operator is constexpr.

struct S {
  constexpr S() {}
  S& operator=(const S&) = default;
  S& operator=(S&&) = default;
};

struct U {
  constexpr U& operator=(const U&) = default;
  constexpr U& operator=(U&&) = default;
};

constexpr void
g ()
{
  S a;
  S b;
  b = a;
  b = S{};

  U u, v;
  u = v;
  u = U{};
}

static_assert ((g(), true), "");
