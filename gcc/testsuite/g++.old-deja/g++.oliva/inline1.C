// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// Derived from GNU lilypond.

// crash test

struct foo {
  foo();
  foo(const foo&);
  ~foo();
};

struct bar {
  foo foo_member;
  bar();
  bar(const bar&);
  // ~bar();
};

struct baz {
  void error (bar s);
};

void fail() __attribute__((noreturn));

void baz::error (bar s) {
  fail();
}
