// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// bug report by Martin Sebor <sebor@roguewave.com>
// from C++ Standard [temp.expl.spec]/5

/* Members of explicitly specialized template classes shall not be
   defined with template-specialization syntax.  The example in the
   Standard contains a definition of a member function of the
   explicitly specialized class template, but the paragraph refers to
   members in general, not only member functions.  */

template<class T> struct A {};

template<> struct A<int> {
  static const bool a, b;
};

const bool A<int>::a; // ok
template<> const bool A<int>::b; // ERROR - bad specialization - XFAIL *-*-*
