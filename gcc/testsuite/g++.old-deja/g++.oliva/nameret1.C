// Build don't link:
// Copyright (C) 1999, 2000, 2002 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// distilled from libg++'s Rational.cc

// Special g++ Options: -Wno-deprecated

inline int bar () return r {} // ERROR - 

int foo () {
  return bar ();
}
