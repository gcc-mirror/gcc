// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Chris McKay <chris.mckay@solipsys.com>

// Used to fail with dwarf debugging.
// crash test

template <class T = void>
struct foo {
  static const int ELEMENTS = 1;
  int bar[ELEMENTS];
};
foo<> bar;
