// PR c++/79092
// { dg-options -std=c++17 }

template<auto V> struct val {};

struct type : val<0>, val<0u> {};
