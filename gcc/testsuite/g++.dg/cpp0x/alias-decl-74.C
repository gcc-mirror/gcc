// PR c++/104108
// { dg-do compile { target c++11 } }

template<template<const int&> class T>
struct S {
  static int m_parameter;
  template<template<const int&> class TT>
  using U = TT<m_parameter>;
};
