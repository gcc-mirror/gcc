// PR c++/48969
// { dg-do compile { target c++11 } }
// { dg-options "-ftemplate-depth=10" }

template<unsigned int N> struct Pair { };
struct Foo { enum { Mask = 1 }; } foo;
template<typename A, typename B> class Pair<A::Mask | B::Mask> // { dg-error "depth" }
operator|(const A &, const B &)
{ }

Pair<Foo::Mask> f = foo|foo;	// { dg-message "from here" }

// { dg-prune-output "compilation terminated" }
