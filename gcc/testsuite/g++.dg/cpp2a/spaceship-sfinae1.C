// { dg-do compile { target c++20 } }

// missing #include <compare>
template <class T, T x = (T() <=> T()) == 0>
void f(T);
constexpr int f(...) { return 42; }
constexpr int i = f(24);
