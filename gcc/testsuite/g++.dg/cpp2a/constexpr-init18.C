// PR c++/96282
// { dg-do compile { target c++20 } }

struct e { bool v = true; bool w; };

template<int N>
struct b { e m[N][N]; };

template<int N>
struct t : b<N> { constexpr t() : b<N>() {} };

constexpr t<1> h1;
static_assert(h1.m[0][0].w == false);

constexpr t<42> h2;
static_assert(h2.m[17][17].w == false);
