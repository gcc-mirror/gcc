// PR c++/84080
// { dg-do compile { target c++14 } }

template <int i, typename T> T foo();

template <> auto foo<0>() { return 42; } // { dg-error "does not match" }
