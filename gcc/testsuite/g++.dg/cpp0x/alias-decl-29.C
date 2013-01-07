// Origin: PR c++/52343
// { dg-do compile { target c++11 } }

template<typename>
using A = int;

template<template<class> class>
struct B {};

B<A> b;
