// { dg-do compile { target c++14 } }

#define SA(X) static_assert((X),#X)

constexpr int f(int i) { { int j = i+1; return j; } }

constexpr int i = f(41);

struct S
{
  constexpr S() { { constexpr int j = 17; SA(j == 17); } }
};

SA(i==42);
