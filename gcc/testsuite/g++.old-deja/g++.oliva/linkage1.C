// Copyright 2002 Free Software Foundation

// Derived by Alexandre Oliva <aoliva@redhat.com> from code posted by
// Mark Mitchell <mark@codesourcery.com>

// Build don't run:

// Additional sources: linkage1-main.cc

// Verify that a member of a class is given global linkage when it's a
// member of a function whose name is taken from a typedef, by
// checking that another translation unit can call it.  We don't do
// the right things on functions, but we do on data members.

// excess errors test - XFAIL *-*-*

typedef struct {
  void f();
} S;

void S::f() {}
