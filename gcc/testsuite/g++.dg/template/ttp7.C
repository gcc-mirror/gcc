// { dg-do compile }
// Contributed by Andrew Pinski <pinskia at gcc dot gnu dot org>
// PR c++/13810: ICE while parsing invalid default argument for a 
//   template template parameter.

struct X;
template<int> struct A {};   

template<template<int> class = X > struct B1 {};     // { dg-error "as a default value" }
template<template<int> class = A<0> > struct B2 {};  // { dg-error "as a default value" }

template <typename T>
struct S {
  template <template <typename> class = S>   struct I1 {};  // { dg-error "as a default value" }
  template <template <typename> class = ::S> struct I2 {};
};
