// PR c++/51526
// { dg-do compile { target c++11 } }

const int j = 42;

struct S {
  int i;
  constexpr S(int i) : i(i) {}
  constexpr S() : S(j) {}
};

constexpr S s{};

#define SA(X) static_assert((X),#X)
SA(s.i == 42);
