// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Harri Porten <porten@tu-harburg.de>

struct A {
  A() : x; // ERROR - missing body
};

struct B {
  void m() {}
};

struct C {
  // The error message below says it is within A::B::m()!
  void n() {}
};
