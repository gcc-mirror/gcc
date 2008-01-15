// PR c++/34052
template<typename T = int, typename U> class C; // { dg-error "no default argument" }

template<template<typename T = int, typename U> class C> struct X; // { dg-error "no default argument" }

