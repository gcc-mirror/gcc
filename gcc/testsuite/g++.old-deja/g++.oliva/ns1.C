// Build don't link:

// Copyright (C) 1999, 2000 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Dima Volodin <dvv@dvv.ru>

void foo();
namespace bar {
  class baz {
    friend void ::foo(); // gets bogus error - parse error
  };
}
