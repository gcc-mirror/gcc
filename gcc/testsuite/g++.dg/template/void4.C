//PR c++/28638

template<void> struct A;  // { dg-error "not a valid type" }

template<template<int> class> struct B {};

B<A> b;                  // { dg-error "template|invalid type" }
