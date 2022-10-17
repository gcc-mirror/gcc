// PR c++/99501
// { dg-do compile { target c++20 } }

template<auto> struct X;

template<auto V> requires requires{V.a;} struct X<V>;
template<auto V> requires requires{V.b;} struct X<V>;

template<auto V> requires requires{V.a;} struct X<V> { static const bool v = false; };
template<auto V> requires requires{V.b;} struct X<V> { static const bool v = true; };

struct A { int a; };
static_assert(!X<A{}>::v);

struct B { int b; };
static_assert(X<B{}>::v);
