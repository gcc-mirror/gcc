// { dg-do assemble  }

// Copyright (C) 1999, 2000 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// simplified from bug report by Paul Burchard <burchard@pobox.com>

template<class> struct A {};
template<template<class> class T> struct B {
  B() {
    T<B>(); // { dg-bogus "" } conversion from int to non-scalar
  }
};
B<A> foo;
