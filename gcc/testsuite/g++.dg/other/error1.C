// { dg-do compile }

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 90, stupid error message `(this + 160)'

class foo {
  public:
  int fudge[40];
  int bar [40];
  inline int access(int i) {
    return bar(i);  // { dg-error "call to non-function `foo::bar'" "" }
  }
};
