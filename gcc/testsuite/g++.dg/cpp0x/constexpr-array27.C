// PR c++/96282
// { dg-do compile { target c++11 } }

struct e { bool v = true; e *p = this; };

template<int N>
struct b { e m[N][N]; };

template<int N>
struct t : b<N> { constexpr t() : b<N>() {} };

constexpr t<1> h1;
constexpr t<42> h2;
