// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<int i> class X { /* ... */ };
X< 1>2 > x1;    // // { dg-error "numeric constant" }
X<(1>2)> x2;    // Okay.

template<class T> class Y { /* ... */ };
Y<X<1>> x3;     // Okay, same as "Y<X<1> > x3;".
Y<X<6>>1>> x4;  // { dg-error "numeric constant" }
Y<X<(6>>1)>> x5;  // Okay
