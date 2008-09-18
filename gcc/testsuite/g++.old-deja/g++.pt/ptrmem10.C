// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2000 <nathan@codesourcery.com>

// A pointer to member can only be formed by `&T::m', however, other forms
// are ok for pointer to static member. Thus the error can only be determined
// after overload resolution. In template deduction, this can disambiguate
// otherwise ambiguous cases.

struct A
{
  static int f (int);
  int f (short);
  void baz ();
};

template <typename T> void foo (int (*)(T));      // { dg-message "candidate" } 
template <typename T> void foo (int (A::*)(T));   // { dg-message "note" } candidate


void A::baz ()
{
  foo (&A::f);  // { dg-error "ambiguous" } 
  foo (A::f);
  foo (&(A::f));
  foo (f);
  foo (&f);
}
