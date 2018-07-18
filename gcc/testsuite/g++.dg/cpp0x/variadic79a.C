// PR c++/33213
// { dg-do compile { target c++11 } }
// { dg-options -fnew-ttp-matching }

template<template<typename> class...> struct A;

template<template<typename...> class... B> struct A<B...> {};
