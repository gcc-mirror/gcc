// PR c++/47721
// { dg-options -std=c++0x }

// template type parameter friend:

template<class W>
class Q
{
  static const int I = 2;
public:
  friend W;
};

struct B
{
  int ar[Q<B>::I];
};

// bonus template template parameter friend:

template <class T> struct A;

template<template <class> class W>
class P
{
  static const int I = 2;
public:
  // I'm not sure this is well-formed, but I can't find anything
  // that says otherwise.
  template <class T> friend class W;
};

template <class T>
struct A
{
  int ar[P<A>::I];
};

A<int> a;

