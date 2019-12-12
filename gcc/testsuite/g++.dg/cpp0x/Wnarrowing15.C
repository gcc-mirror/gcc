// PR c++/78244
// { dg-do compile { target c++11 } }

template <typename T>
auto f1() -> decltype(int{2.0}, void()) { } // { dg-error "narrowing conversion" }

template <typename T>
auto f2() -> decltype(int{2.0}) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f3() -> decltype(void(), int{2.0}) { return 1; } // { dg-error "narrowing conversion" }

template <typename T>
auto f4() -> decltype((int{2.0})) { return 1; } // { dg-error "narrowing conversion" }
