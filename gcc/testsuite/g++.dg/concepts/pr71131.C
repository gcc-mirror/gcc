// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<template<typename> class T>
concept C = true;

C c = 1;  // { dg-error "does not constrain a type" }
