//PR c++/28861

struct A
{
  template<template<int> class B> struct B<0>;  // { dg-error "name of class shadows" }
};
