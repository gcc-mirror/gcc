// DR 1453
// { dg-do compile { target c++11 } }

struct S {
  constexpr S() : n{} { }
  volatile int n;
};

constexpr S s;  // { dg-error "literal" }

struct Z {
  volatile int m;
};

struct T {
  constexpr T() : n{} { }
  Z n;
};

constexpr T t;  // { dg-error "literal" }

struct U : Z {
  constexpr U() : Z{} { }
};

constexpr U u;  // { dg-error "literal" }
