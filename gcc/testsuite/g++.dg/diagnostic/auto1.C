// PR c++/86915
// { dg-do compile { target c++17 } }

template<auto [1]> struct S; // { dg-error "creating array of .auto." }
