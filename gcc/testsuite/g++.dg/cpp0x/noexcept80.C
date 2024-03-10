// PR c++/110106
// { dg-do compile { target c++11 } }

template<int> struct S
{
};

struct G {
  G(S<0>);
};

void y(S<0> s) noexcept(noexcept(G{s}));
