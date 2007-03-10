// { dg-options "-std=gnu++0x" }

template<class T> class A { /* ... */ };
template<class T, class U = T> class B { /* ... */ };
template<class... Types> class C { /* ... */ };

template<template<class> class P> class X { /* ... */ };
template<template<class...> class Q> class Y { /* ... */ };

X<A> xA; // okay
X<B> xB; // { dg-error "mismatch" }
// { dg-error "expected a template" "" { target *-*-* } 11 }
// { dg-error "invalid type" "" { target *-*-* } 11 }
X<C> xC; // { dg-error "mismatch" }
// { dg-error "expected a template" "" { target *-*-* } 14 }
// { dg-error "invalid type" "" { target *-*-* } 14 }
Y<A> yA; // { dg-error "mismatch" }
// { dg-error "expected a template" "" { target *-*-* } 17 }
// { dg-error "invalid type" "" { target *-*-* } 17 }
Y<B> yB; // { dg-error "mismatch" }
// { dg-error "expected a template" "" { target *-*-* } 20 }
// { dg-error "invalid type" "" { target *-*-* } 20 }
Y<C> yC; // okay
