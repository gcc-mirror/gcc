// PR c++/33210

template<int> struct A;

template<template<typename> class B> A<B<int>::x> operator() (); // { dg-error "51:.A<B<int>::x> operator\\(\\)\\(\\). must be a nonstatic member function" }
