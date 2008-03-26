// { dg-options "-std=c++0x" }

template<template<typename...> class TT>
struct X { };

template<typename T, typename U> struct pair { };

X<pair> x;
