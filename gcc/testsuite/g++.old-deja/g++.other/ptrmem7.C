// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2000 <nathan@codesourcery.com>

// A pointer to member can only be formed by `&T::m', however, other forms
// are ok for pointer to static member. Thus the error can only be determined
// after overload resolution.

struct A
{
  static int ns (short);
  static int ns (float);
  int ns (int);
  int ns (double);
  int single (int);
  static int sole (short);
  void foo ();
};
void A::foo ()
{
  int (A::*ptr1) (int) = &A::ns;
  int (A::*ptr2) (int) = A::ns;           // ERROR - not ptr mem
  int (A::*ptr3) (int) = &ns;             // ERROR - not ptr mem
  int (A::*ptr4) (int) = ns;              // ERROR - not ptr mem

  int (*ptr5) (short) = &A::ns;
  int (*ptr6) (short) = A::ns;
  int (*ptr7) (short) = &ns;
  int (*ptr8) (short) = ns;

  int (A::*ptr11) (int) = &A::single;
  int (A::*ptr12) (int) = A::single;      // ERROR - not ptr mem
  int (A::*ptr13) (int) = &single;        // ERROR - not ptr mem
  int (A::*ptr14) (int) = single;         // ERROR - not ptr mem

  int (A::*ptr20) (int) = &(A::ns);       // ERROR - not ptr mem
  int (A::*ptr21) (int) = &(A::single);   // ERROR - not ptr mem

  int (*ptr31) (short) = &A::sole;
  int (*ptr32) (short) = A::sole;
  int (*ptr33) (short) = &sole;
  int (*ptr34) (short) = sole;
  int (*ptr41) (short) = &(A::sole);
  int (*ptr43) (short) = &(sole);
}
