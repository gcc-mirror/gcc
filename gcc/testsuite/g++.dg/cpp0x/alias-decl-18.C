// Origin: PR c++/51541
// { dg-do compile { target c++11 } }

template<typename Z> using ::T = void(int n); // { dg-error "" }
template<typename Z> using operator int = void(int n); // { dg-error "" }
template<typename Z> using typename U = void; // { dg-error "" }
template<typename Z> using typename ::V = void(int n); // { dg-error "" }
template<typename Z> using typename ::operator bool = void(int n); // { dg-error "39:declaration" }
// { dg-error "expected" "" { target *-*-* } .-1 }
using foo __attribute__((aligned(4)) = int; // { dg-error "" }
