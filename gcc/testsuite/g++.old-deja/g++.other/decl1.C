// Build don't link:
// Based on a test case by Phil Blecker <tmwg@inxservices.com>

// excess errors test - XFAIL *-*-*

int foo(int);
int bar() {
  int baz(int(foo(0)));
  int foo = baz;
}
