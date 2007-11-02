// PR c++/30897

template<template <typename T, typename = T > class U> struct A
{
  template<int> U<int> foo();
};
