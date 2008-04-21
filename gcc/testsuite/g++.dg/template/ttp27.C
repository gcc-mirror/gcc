// PR c++/35678

template<typename T, T> struct A;
template<typename> struct B; 
template<template<typename T, T> class U> struct B<U<char, 'h'> > {};
B<A<char,'h'> > x;
