// PR c++/28363
// { dg-do compile }

template<typename T, template<int> class = T>  // { dg-error "invalid use of type" }
struct A;

typedef int I;
template<template<int> class = I>  // { dg-error "invalid use of type" }
struct B;

struct S;
template<template<int> class = S>  // { dg-error "invalid use of type" }
struct C;
