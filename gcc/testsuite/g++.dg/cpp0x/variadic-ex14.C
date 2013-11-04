// { dg-options "-std=gnu++11" }

template<class T> class A { /* ... */ };
template<class T, class U = T> class B { /* ... */ };
template<class... Types> class C { /* ... */ };

template<template<class> class P> class X { /* ... */ };
template<template<class...> class Q> class Y { /* ... */ };

X<A> xA; // okay
X<B> xB; // { dg-error "mismatch" "mismatch" }
// { dg-error "expected a template" "expected" { target *-*-* } 11 }
// { dg-error "invalid type" "invalid" { target *-*-* } 11 }
X<C> xC; // { dg-error "mismatch" "mismatch" }
// { dg-error "expected a template" "expected" { target *-*-* } 14 }
// { dg-error "invalid type" "invalid" { target *-*-* } 14 }
Y<A> yA;
Y<B> yB;
Y<C> yC; // okay
