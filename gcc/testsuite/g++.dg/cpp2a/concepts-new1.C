// PR c++/108219
// { dg-do compile { target c++20 } }

template<class T>
concept C = requires { new T[1]{{ 42 }}; };

template<class T>
concept D = requires { new T[2][1]{{{ 42 }}, {{ 42 }}}; };

struct A { A(int); };

static_assert(C<A>);
static_assert(D<A>);
