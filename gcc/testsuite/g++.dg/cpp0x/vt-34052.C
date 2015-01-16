// { dg-do compile { target c++11 } }
template<typename... T, typename = T> struct A {}; // { dg-error "parameter pack" }


template<template<typename... T, typename = T> class U> struct B // { dg-error "parameter pack" }
{
  template<int> U<int> foo(); // { dg-error "mismatch|constant|wrong|invalid" }
};
