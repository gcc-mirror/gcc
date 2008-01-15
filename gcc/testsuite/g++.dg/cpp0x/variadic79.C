// PR c++/33213
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<template<typename> class...> struct A;

template<template<typename...> class... B> struct A<B...> {}; // { dg-error "mismatch|'template<class ...> class ... B ...'" }
