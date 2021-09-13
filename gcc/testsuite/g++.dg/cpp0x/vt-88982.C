// PR c++/88982
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fchecking" }
// { dg-ice "tsubst_pack_expansion" }

template<typename...Ts> struct A {
  template<template<typename, Ts = 0> class ...Cs, Cs<Ts> ...Vs> struct B {
    B() {
    }
  };
};

template<typename, int> using Int = int;
template<typename, short> using Char = char;
A<int, short>::B<Int, Char> b;
