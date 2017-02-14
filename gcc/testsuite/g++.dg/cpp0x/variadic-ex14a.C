// CWG 150: Matching of template template-arguments excludes compatible
// templates
// { dg-options -fnew-ttp-matching }

template<class T> class A { /* ... */ };
template<class T, class U = T> class B { /* ... */ };
template<template<class> class P> class X { /* ... */ };
X<A> xa; // OK
X<B> xb; // OK since P0522R0

#if __cpp_variadic_templates
template <class ... Types> class C { /* ... */ };
template<template<class ...> class Q> class Y { /* ... */ };
X<C> xc; // OK since P0522R0
Y<A> ya; // OK
Y<B> yb; // OK
Y<C> yc; // OK
#endif

#if __cpp_template_auto
template<auto n> class D { /* ... */ };
template<template<int> class R> class Z { /* ... */ };
Z<D> zd; // OK
#endif
