// Build don't link:

// Copyright (C) 2000 Free Software Foundation

// by Alexandre Oliva <aoliva@cygnus.com>
// distilled from libg++'s Fix.cc

// crash test - XFAIL i*86-*-*

struct Integer {
  ~Integer () {}
};

void foo (const Integer& y);
Integer bar (const Integer& x);

void show (const Integer& x) {
  foo (bar (x));
}
