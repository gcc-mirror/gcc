// PR c++/33213
// { dg-do compile { target c++11 } }

template<template<typename> class...> struct A;

template<template<typename...> class... B> struct A<B...> {}; // { dg-error "mismatch|'template<class ...> class ... B ...'" }
