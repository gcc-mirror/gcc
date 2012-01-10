// Origin: PR c++/51541
// { dg-options -std=c++11 }

template<typename Z> using ::T = void(int n); // { dg-error "" }
template<typename Z> using operator int = void(int n); // { dg-error "" }
template<typename Z> using typename U = void; // { dg-error "" }
template<typename Z> using typename ::V = void(int n); // { dg-error "" }
template<typename Z> using typename ::operator bool = void(int n); // { dg-error "" }
using foo __attribute__((aligned(4)) = int; // { dg-error "" }
