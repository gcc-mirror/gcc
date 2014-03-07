// { dg-do compile { target c++11 } }

constexpr int f(int i) { return i; }
#define SA(X) static_assert (X, #X)
SA(noexcept(f(42)));
int j;
SA(!noexcept(f(j)));
