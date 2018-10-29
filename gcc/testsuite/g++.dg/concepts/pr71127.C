// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<template<typename> class T>
concept bool C = T<int>::value;
C c = 1;  // { dg-error "invalid reference to concept" }
