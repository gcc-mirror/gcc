// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<template<typename> class T>
concept bool C = true;

C c = 1;  // { dg-error "does not constrain a type" }
