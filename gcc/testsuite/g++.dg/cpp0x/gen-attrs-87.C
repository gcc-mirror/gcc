// PR c++/118773
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <unsigned N>
using T = char[4] [[gnu::aligned (N)]];
T<2> t;
template <unsigned N>
using U = char *[[gnu::aligned (N)]]*;
U<__alignof (char *)> u;
