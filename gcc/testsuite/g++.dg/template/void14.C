// PR c++/36411
// { dg-do compile }

template<template<void> class> struct A // { dg-error "not a valid type" }
{
  template<template<int> class T> A<T> foo(); // { dg-error "mismatch|expected|invalid" }
};
