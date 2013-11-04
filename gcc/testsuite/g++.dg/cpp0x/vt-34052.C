// { dg-options "-std=c++11" }
template<typename... T, typename = T> struct A {}; // { dg-error "must be at the end" }


template<template<typename... T, typename = T> class U> struct B // { dg-error "must be at the end" }
{
  template<int> U<int> foo(); // { dg-error "mismatch|constant|invalid|invalid" }
};
