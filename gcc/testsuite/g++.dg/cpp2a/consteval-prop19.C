// P2564R3
// { dg-do compile { target c++20 } }

consteval int g(int p) { return p; }
template<typename T> constexpr auto f(T) { return g; }
int r = f(1)(2);      // proposed ok
int s = f(1)(2) + r;  // { dg-error "call to consteval function|returns address of immediate function" }
