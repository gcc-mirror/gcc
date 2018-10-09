// { dg-do compile { target c++11 } }
// { dg-additional-options "-fconcepts" }

template<typename> using A = auto;  // { dg-error "30:.auto. not allowed in alias declaration" }

template<template<typename> class> struct B {};

B<A> b;
