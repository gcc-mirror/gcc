// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Sep 2006 <nathan@codesourcery.com>

// PR 23287: Failure to parse dependent dtor name
// Origin:Wolfgang Bangerth  <bangerth@dealii.org>


template <class T> struct A {}; 
 
template <class T> void f(A<T> *ptr) { 
  ptr->~A(); 
}

template void f<void> (A<void> *);

