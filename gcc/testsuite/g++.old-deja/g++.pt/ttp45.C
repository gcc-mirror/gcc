// Build don't link:

template<class M, class T> struct temp2;
template<template<class> class M, class T> struct temp2<M<T>, T> {};

template<class M> struct temp1;
template<template<class> class M, class T> struct temp1<M<T> > {};
