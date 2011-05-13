// PR c++/48969
// { dg-options -std=c++0x }

template<unsigned int N> struct Pair { };
struct Foo { enum { Mask = 1 }; } foo;
template<typename A, typename B> class Pair<A::Mask | B::Mask>
operator|(const A &, const B &)
{ }

Pair<Foo::Mask> f = foo|foo;
