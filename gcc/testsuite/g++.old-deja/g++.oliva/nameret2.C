// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// distilled from libg++'s Integer.cc

// Special g++ Options: -O1
// crash test - XFAIL *-*-*

inline int bar () return r {}

int& foo (int& x) {
  bar ();
  return x;
}
