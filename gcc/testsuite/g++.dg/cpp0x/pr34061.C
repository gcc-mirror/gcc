// PR c++/34061
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<template<int> class ...T> struct A : T<0> {}; // { dg-error "not expanded|T" }
