// DR 1453
// { dg-do compile { target c++11 } }

struct S {
  constexpr S() : n{} { }
  volatile int n;
};

static_assert(!__is_literal_type(S), "");

struct Z {
  volatile int m;
};

struct T {
  constexpr T() : n{} { }
  Z n;
};

static_assert(!__is_literal_type(T), "");

struct U : Z {
  constexpr U() : Z{} { }
};

static_assert(!__is_literal_type(U), "");
