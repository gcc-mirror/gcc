// PR c++/78244
// { dg-do compile { target c++11 } }

struct S { int d; };

template <typename T>
auto f1() -> decltype(S{2.0}, void()) { } // { dg-error "narrowing conversion" }

template <typename T>
auto f2() -> decltype(S{2.0}, 1) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f3() -> decltype(void(), S{2.0}, 1) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f4() -> decltype((S{2.0}, 1)) { return 1; } // { dg-error "narrowing conversion" }
