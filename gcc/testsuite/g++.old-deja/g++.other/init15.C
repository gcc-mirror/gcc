// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 21 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 136 from
// language specific constants caused the backend's constant caching machinery
// to fall over.

struct A {
  const char *name;
  int reserved;
  int a;
  int b;
  void (A::*func)();
  void Fn ();
};

void Interpret() {
  struct A cmd_list =
    {"a",0,0, 0,&A::Fn}
  ;
}
