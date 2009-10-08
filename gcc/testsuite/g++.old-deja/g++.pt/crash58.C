// { dg-do assemble  }
// 
// Copyright (C) 2000, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sept 2000 <nathan@codesourcery.com>

// bug 147. We ICE'd on an unprocessed LOOKUP_EXPR during tsubsting

namespace EManip {
    template <class T> void do_assign(T* d);  // { dg-error "" } candidate
}
template <class T> void do_assign(T* d);    // { dg-error "" } candidate

template <class T>
struct MatrixC
{
  void foo () {
    EManip::do_assign<T> (0);
    &EManip::do_assign<T>;	// { dg-bogus "" } unresolved
    &do_assign<T>;		// { dg-bogus "" } unresolved
    EManip::do_assign<T>;       // { dg-bogus "" } unresolved
    do_assign<T>;               // { dg-bogus "" } unresolved
  }
};
void foo(MatrixC <double> *ptr)
{
  EManip::do_assign<double>;    // { dg-bogus "" } unresolved
  &EManip::do_assign<double>;	// { dg-bogus "" } unresolved
  ptr->foo ();
  void (*p1) (int *) = &do_assign<double>;       // { dg-error "" } cannot convert
  void (*p2) (int *) = &EManip::do_assign<double>; // { dg-error "" } cannot convert
  void (*p3) (int *) = &do_assign;
  void (*p4) (int *) = &EManip::do_assign;
}
