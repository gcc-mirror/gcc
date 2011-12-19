// PR c++/51526
// { dg-options -std=c++0x }

const int j = 42;

struct S {
  int i;
  constexpr S(int i) : i(i) {}
  constexpr S() : S(j) {}
};

constexpr S s{};

#define SA(X) static_assert((X),#X)
SA(s.i == 42);
