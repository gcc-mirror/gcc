// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// bug report by Martin Sebor <sebor@roguewave.com>
// based on C++ Standard example in [temp.expl.spec]/5

/* Members of explicitly specialized template classes shall not be
   defined with template-specialization syntax.  The example in the
   Standard contains a definition of a member function of the
   explicitly specialized class template, but the paragraph refers to
   members in general, not only member functions.  */

template<class T> struct A {};

template<> struct A<int> {
  static bool a, b;
};

bool A<int>::a = true; // ok
template<> bool A<int>::b = false; // { dg-error "" "" { xfail *-*-* } } 
