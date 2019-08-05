// PR c++/77575
// { dg-do compile { target c++11 } }

template<template <class> class> struct meow {};
template<class T> using kitty = T&;

meow<kitty> u;
