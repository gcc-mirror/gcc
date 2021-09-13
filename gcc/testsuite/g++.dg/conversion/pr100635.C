// PR c++/100635
// { dg-do compile }
// { dg-additional-options "-Wno-volatile" { target c++2a } }

struct S { };
volatile S v();
const volatile S& svol = v(); // { dg-error "cannot bind lvalue reference of type 'const volatile S&' to an rvalue of type 'volatile S'" }

#if __cplusplus >= 201103L
volatile int&& declvol();
const volatile int& voli = declvol(); // { dg-error "cannot bind lvalue reference of type 'const volatile int&' to an rvalue of type 'volatile int'" "" { target c++11} }
#endif
