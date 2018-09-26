// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<template<typename> class T>
concept bool C = true;
C c = 1;  // { dg-error "invalid reference to concept" }
