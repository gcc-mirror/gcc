// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Dima Volodin <dvv@dvv.ru>
// variation of ns1.C

void foo();
namespace bar {
  using ::foo;
  class baz {
    friend void foo(); // { dg-bogus "" "" { xfail *-*-* } }  - conflict - 
  };
}
