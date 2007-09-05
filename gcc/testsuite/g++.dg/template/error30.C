// PR c++/33210

template<int> struct A;

template<template<typename> class B> A<B<int>::x> operator() (); // { dg-error "A<B<int>::x>" }
