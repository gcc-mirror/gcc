// PR c++/86915
// { dg-do compile { target c++17 } }
// Allowed since DR2397.

template<auto [1]> struct S;
