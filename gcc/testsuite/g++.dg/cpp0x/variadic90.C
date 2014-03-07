// { dg-do compile { target c++11 } }

template<template<typename...> class TT>
struct X { };

template<typename T, typename U> struct pair { };

X<pair> x;
