// Test SFINAE for a non-constant consteval call.
// { dg-do compile { target c++20 } }

consteval int deref(const int* x) { return *x; }

template<const int* P, int = deref(P)> // { dg-bogus "null pointer" }
constexpr int f(int) { return 0; }

template<const int* P>
constexpr int f(...) { return 1; }

static_assert(f<nullptr>(0) == 1);
