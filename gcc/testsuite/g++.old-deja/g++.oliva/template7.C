// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Paul Burchard <burchard@pobox.com>

// crash test - XFAIL *-*-*

template<class> struct A {};
template<template<class> class T> struct B {
  B() {
    T<B>();
  }
};
B<A> foo;
