// { dg-do compile { target c++11 } }
template<int... Values>
struct int_vec {};

template<int... Values>
struct int_vec<0, (Values+1)...> {}; // { dg-error "not deducible" }
