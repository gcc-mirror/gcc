// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// based on bug report by Philippe Bouchard <boucp00@DMI.USherb.CA>

struct foo {
  template <class>
  void bar() = 0; // { dg-error "" "" { xfail *-*-* } } invalid initializer - 
};
