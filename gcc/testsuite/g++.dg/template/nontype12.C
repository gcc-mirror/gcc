// PR c++/20172
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

template<typename T> struct A
{
  template<T> int foo();                        // { dg-error "double" "" { target c++17_down } }
  template<template<T> class> int bar();        // { dg-error "double" "" { target c++17_down } }
  template<T> struct X;                         // { dg-error "double" "" { target c++17_down } }
};

A<char>   a1;
A<double> a2;                                   // { dg-message "required" "" { target c++17_down } }

template<typename T> struct B
{
  template<double> int foo();                   // { dg-error "double" "" { target c++17_down } }
  template<template<double> class> int bar();   // { dg-error "double" "" { target c++17_down } }
  template<double> struct X;                    // { dg-error "double" "" { target c++17_down } }
};

template<void> int foo();                       // { dg-error "void" }
template<template<void> class> int bar();       // { dg-error "void" }
template<void> struct X;                        // { dg-error "void" }

template<typename T> struct C
{
  template<T> int foo();                        // { dg-error "double" "" { target c++17_down } }
};

template<typename T> int baz(T) { C<T> c; return 0;}  // { dg-message "required" "" { target c++17_down } }

void foobar()
{
  baz(1.2);                                     // { dg-message "required" "" { target c++17_down } }
}
