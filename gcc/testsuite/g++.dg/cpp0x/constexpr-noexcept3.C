// { dg-options -std=c++0x }

constexpr int f(int i) { return i; }
#define SA(X) static_assert (X, #X)
SA(noexcept(f(42)));
int j;
SA(!noexcept(f(j)));
