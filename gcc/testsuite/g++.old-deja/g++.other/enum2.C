// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Jun 1999 <nathan@acm.org>

// We'd like the enum location to be its open brace.

enum thing
{ // ERROR - previous def
  val1
};

enum thing
{ // ERROR - multiple def
  val2
};
