// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>

// based on bug report by Dima Volodin <dvv@dvv.ru>

void foo();
namespace bar {
  class baz {
    friend void ::f(); // gets bogus error - parse error - XFAIL *-*-*
  };
}
