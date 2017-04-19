// { dg-do compile { target c++11 } }
// { dg-options -fno-new-ttp-matching }
template<class T> class A { /* ... */ };
template<class T, class U = T> class B { /* ... */ };
template<class... Types> class C { /* ... */ };

template<template<class> class P> class X { /* ... */ };
template<template<class...> class Q> class Y { /* ... */ };

X<A> xA; // okay
X<B> xB; // { dg-error "mismatch" "mismatch" }
// { dg-message "expected a template" "expected" { target *-*-* } .-1 }
X<C> xC; // { dg-error "mismatch" "mismatch" }
// { dg-message "expected a template" "expected" { target *-*-* } .-1 }
Y<A> yA;
Y<B> yB;
Y<C> yC; // okay
