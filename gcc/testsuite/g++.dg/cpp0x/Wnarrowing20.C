// PR c++/78244
// { dg-do compile { target c++11 } }

struct S { S(int); int d; };

template <typename T>
auto f1() -> decltype(S{2.0}, void()) { } // { dg-error "narrowing conversion" }

template <typename T>
auto f2() -> decltype(S{2.0}, 1) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f3() -> decltype(void(), S{2.0}, 1) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f4() -> decltype((S{2.0}, 1)) { return 1; } // { dg-error "narrowing conversion" }

// Test OVERLOAD in a template.
int id(int v) { return v; }
double id(double v) { return v; }

template <typename T>
auto f5(double v) -> decltype((S{id(v)}, 1)) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f6(int v) -> decltype((S{id(v)}, 1)) { return 1; }
