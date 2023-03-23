// PR c++/105481
// { dg-do compile { target c++11 } }

template<unsigned> struct uint;
template<unsigned N> uint<N> f(const uint<N> &);
template<unsigned N, typename T, typename = uint<N>> uint<N> f(T);
using X = uint<1>;
X (*fp)(X const &) = f;
