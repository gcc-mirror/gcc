// { dg-options "-std=gnu++0x" }
template<int... Values>
struct int_vec {};

template<int... Values>
struct int_vec<0, (Values+1)...> {}; // { dg-error "involves template parameter" }
