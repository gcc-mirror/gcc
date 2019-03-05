// PR c++/28860
// { dg-do compile }

template<template<int> class A>
class A<0>;  // { dg-error "declaration does not declare anything" }

template<template<int> class B>
class B<0> {};  // { dg-error "shadows template template parameter" }
