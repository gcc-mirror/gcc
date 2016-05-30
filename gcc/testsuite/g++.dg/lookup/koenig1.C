// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Mar 2003 <nathan@codesourcery.com>

// PR 10026. We ICE'd

class X;

void foo() {
  X x(1); // { dg-error "incomplete type" "" }
  bar(x); // { dg-error "3:'bar' was not declared" "" }
}
