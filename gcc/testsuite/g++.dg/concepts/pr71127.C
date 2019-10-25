// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<template<typename> class T>
concept bool C = T<int>::value;

C c = 1;  // { dg-error "does not constrain a type" }
