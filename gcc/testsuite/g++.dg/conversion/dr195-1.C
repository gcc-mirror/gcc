// { dg-options "" }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Oct 2004 <nathan@codesourcery.com>

// DR 195 will allow conversions between function and object pointers
// under some circumstances. It is in drafting, so we don't implement
// it (yet).

// this checks we are silent when not being pedantic.

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
  pv = pf; // { dg-error "invalid conversion" "" }
  pf = pv; // { dg-error "invalid conversion" "" }
}
