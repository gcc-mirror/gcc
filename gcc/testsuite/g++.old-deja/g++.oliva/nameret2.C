// { dg-do assemble  }
// { dg-options "-O1 -Wno-deprecated -Wno-return-type" }
// Copyright (C) 1999, 2000, 2002 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// distilled from libg++'s Integer.cc


inline int bar () return r {} // { dg-error "" } 

int& foo (int& x) {
  bar ();
  return x;
}
