// { dg-do compile { target c++11 } }

template<class T>
using ref = T&;

int x;

template<template<class> class T, T<int>>
struct X { };

struct Y : X<ref, x> { };
