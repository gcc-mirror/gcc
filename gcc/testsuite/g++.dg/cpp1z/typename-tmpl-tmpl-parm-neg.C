// { dg-do compile }
// { dg-options "" }

template<template<typename> struct X> // { dg-error "expected .class. or .typename. before" }
  struct D {};

template<template<typename> X> // { dg-error "expected .class. or .typename. before" }
  struct E {};

// { dg-error "expected identifier" "expected" { target *-*-* } 4 }
// { dg-error "expected .>." "expected" { target *-*-* } 4 }
