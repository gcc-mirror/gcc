// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Oct 2004 <nathan@codesourcery.com>

// DR 195 allows conversions between function and object pointers
// under some circumstances.

typedef void (*PF)(void);
typedef void *PV;
typedef int *PO;


void foo ()
{
  PF pf;
  PV pv;
  PO po;

  pf = reinterpret_cast <PF>(pv);
  pv = reinterpret_cast <PV>(pf);
  pf = reinterpret_cast <PF>(po); // { dg-error "casting between" "" }
  po = reinterpret_cast <PO>(pf); // { dg-error "casting between" "" }

  pv = pf; // { dg-error "invalid conversion" "" }
  pf = pv; // { dg-error "invalid conversion" "" }
}
