// PR c++/34061
// { dg-do compile { target c++11 } }

template<template<int> class ...T> struct A : T<0> {}; // { dg-error "not expanded|T" }
