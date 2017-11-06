// { dg-do assemble  }
// { dg-options "-Wno-deprecated -Wno-return-type" }
// Copyright (C) 1999, 2000, 2002 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// distilled from libg++'s Rational.cc


inline int bar () return r {} // { dg-error "" } 

int foo () {
  return bar ();
}
