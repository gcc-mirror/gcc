// PR c++/71821
// { dg-do compile { target c++11 } }

template < typename > constexpr int f () {  return 4; }

alignas (f < int >) char c;  // { dg-error "non-integral type" }
