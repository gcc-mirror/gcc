// PR c++/20172
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

template<typename T> struct A
{
  template<T> int foo();                        // { dg-error "double" }
  template<template<T> class> int bar();        // { dg-error "double" }
  template<T> struct X;                         // { dg-error "double" }
};

A<char>   a1;
A<double> a2;                                   // { dg-message "instantiated" }

template<typename T> struct B
{
  template<double> int foo();                   // { dg-error "double" }
  template<template<double> class> int bar();   // { dg-error "double" }
  template<double> struct X;                    // { dg-error "double" }
};

template<void> int foo();                       // { dg-error "void" }
template<template<void> class> int bar();       // { dg-error "void" }
template<void> struct X;                        // { dg-error "void" }

template<typename T> struct C
{
  template<T> int foo();                        // { dg-error "double" }
};

template<typename T> int baz(T) { C<T> c; }     // { dg-message "instantiated" }

void foobar()
{
  baz(1.2);                                     // { dg-message "instantiated" }
}
