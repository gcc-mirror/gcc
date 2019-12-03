// { dg-do assemble  }
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
  int (A::*ptr2) (int) = A::ns;           // { dg-error "pointer to member" "err" }
  // { dg-message "pointer to member" "note" { target *-*-* } .-1 }
  int (A::*ptr3) (int) = &ns;             // { dg-error "pointer to member" "err" }
  int (A::*ptr4) (int) = ns;              // { dg-error "pointer to member" "err" }

  int (*ptr5) (short) = &A::ns;
  int (*ptr6) (short) = A::ns;
  int (*ptr7) (short) = &ns;
  int (*ptr8) (short) = ns;

  int (A::*ptr11) (int) = &A::single;
  int (A::*ptr12) (int) = A::single;      // { dg-error "cannot convert" }
  int (A::*ptr13) (int) = &single;        // { dg-error "pointer to member" }
  int (A::*ptr14) (int) = single;         // { dg-error "cannot convert" }

  int (A::*ptr20) (int) = &(A::ns);       // { dg-error "pointer to member" }
  int (A::*ptr21) (int) = &(A::single);   // { dg-error "32:ISO C\\+\\+ forbids taking the address of an unqualified or parenthesized non-static member function to form a pointer to member" }

  int (*ptr31) (short) = &A::sole;
  int (*ptr32) (short) = A::sole;
  int (*ptr33) (short) = &sole;
  int (*ptr34) (short) = sole;
  int (*ptr41) (short) = &(A::sole);
  int (*ptr43) (short) = &(sole);
}
