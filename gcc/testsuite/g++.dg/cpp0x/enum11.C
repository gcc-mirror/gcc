// PR c++/48969
// { dg-options "-std=c++11 -ftemplate-depth=10" }

template<unsigned int N> struct Pair { };
struct Foo { enum { Mask = 1 }; } foo;
template<typename A, typename B> class Pair<A::Mask | B::Mask>
operator|(const A &, const B &)	// { dg-message "substitution" }
{ }

Pair<Foo::Mask> f = foo|foo;	// { dg-message "no match" }

// { dg-prune-output "note" }
// { dg-prune-output "here" }
// { dg-prune-output "instantiation depth" }
