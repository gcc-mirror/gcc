// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sept 2000 <nathan@codesourcery.com>

// bug 147. We ICE'd on an unprocessed LOOKUP_EXPR during tsubsting

namespace EManip {
    template <class T> void do_assign(T* d);  // ERROR - candidate
};
template <class T> void do_assign(T* d);    // ERROR - candidate

template <class T>
struct MatrixC
{
  void foo () {
    EManip::do_assign<T> (0);
    &EManip::do_assign<T>;
    &do_assign<T>;
    EManip::do_assign<T>;       // WARNING - not a call
    do_assign<T>;               // WARNING - not a call
  }
};
void foo(MatrixC <double> *ptr)
{
  EManip::do_assign<double>;    // WARNING - not a call
  &EManip::do_assign<double>;
  ptr->foo ();
  void (*p1) (int *) = &do_assign<double>;       // ERROR - cannot convert
  void (*p2) (int *) = &EManip::do_assign<double>; // ERROR - cannot convert
  void (*p3) (int *) = &do_assign;
  void (*p4) (int *) = &EManip::do_assign;
}
