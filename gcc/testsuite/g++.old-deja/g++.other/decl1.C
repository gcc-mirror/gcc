// { dg-do assemble  }
// Based on a test case by Phil Blecker <tmwg@inxservices.com>


int foo(int);
int bar() {
  int baz(int(foo(0)));
  int foo = baz;
}
