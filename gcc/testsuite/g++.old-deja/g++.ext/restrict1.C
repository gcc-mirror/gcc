// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 1999 <nathan@acm.org>
// Test our restrict object pointer extension

struct A
{
  void f1() __restrict__;
  void g1(int *__restrict__);
  void f2() __restrict__;
  void g2(int *__restrict__);
  void f3();
  void g3(int *);
  void f4();
  void g4(int *);
};

void A::f1 () __restrict__ {}
void A::g1 (int *__restrict__) {}

void A::f2 () {}
void A::g2 (int *) {}

void A::f3 () __restrict__ {}
void A::g3 (int *__restrict__) {}

void A::f4 () {}
void A::g4 (int *) {}

template <class T> struct X
{
  void f1() __restrict__;
  void g1(int *__restrict__);
  void f2() __restrict__;
  void g2(int *__restrict__);
  void f3();
  void g3(int *);
  void f4();
  void g4(int *);
};

template <class T> void X<T>::f1 () __restrict__ {}
template <class T> void X<T>::g1 (int *__restrict__) {}
template <class T> void X<T>::f2 () {}
template <class T> void X<T>::g2 (int *) {}
template <class T> void X<T>::f3 () __restrict__ {}
template <class T> void X<T>::g3 (int *__restrict__) {}
template <class T> void X<T>::f4 () {}
template <class T> void X<T>::g4 (int *) {}

void fn ()
{
  X<int> g;
  
  g.f1 ();
  g.f2 ();
  g.f3 ();
  g.f4 ();
}
