// PR c++/79092
// { dg-do compile { target c++17 } }

template<auto V> struct val {};

struct type : val<0>, val<0u> {};
