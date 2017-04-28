// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Oct 2004 <nathan@codesourcery.com>

// DR 195 was about allowing conversions between function and object
// pointers under some circumstances.  The issue got resolved for C++11,
// which, in 5.2.10 p8 says that: "Converting a function pointer to an
// object pointer type or vice versa is conditionally-supported."

// This checks we don't warn anymore with -pedantic.

typedef void (*PF)(void);
typedef void *PV;
typedef int *PO;

void foo ()
{
  PF pf;
  PV pv;
  PO po;

  /* the following two will almost definitly be ok with 195.  */
  pf = reinterpret_cast <PF>(pv);
  pv = reinterpret_cast <PV>(pf);

  /* the following two might or might not be ok with 195.  */
  pf = reinterpret_cast <PF>(po);
  po = reinterpret_cast <PO>(pf);

  /* These will never be ok, as they are implicit.  */
  pv = pf; // { dg-error "invalid conversion" }
  pf = pv; // { dg-error "invalid conversion" }
}
