// PR c++/28853
// { dg-do compile }

template<template<int> class A>
int A<0>::i;  // { dg-error "template template parameter" }
