// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Jun 1999 <nathan@acm.org>

// We'd like the enum location to be its open brace.

enum thing
{ // { dg-error "" } previous def
  val1
};

enum thing
{ // { dg-error "" } multiple def
  val2
};
