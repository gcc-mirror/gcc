// { dg-options "-std=c++0x" }

template<template<class> class TT> struct X { };
template<class> struct Y { };
template<class T> using Z = Y<T>;

void f(X<Y>);
void g(X<Z>);

void
foo()
{
    // Below x and y don't have the same type, because Y and Z don't
    // designate the same template ...
    X<Y> y; 
    X<Z> z;

    // ... So these must fail to compile.
    f(z);   // { dg-error "" }
    g(y);   // { dg-error "" }
}

template<class> struct A0 {};
template<class T> using AA0 = A0<T>;
template<class T> using AAA0 = AA0<T>;

void f0(A0<int>);
void
g0()
{
  AA0<int> a;
  AAA0<int> b;
  f0(a);
  f0(b);
}


