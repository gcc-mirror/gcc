// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Nov 2000 <nathan@codesourcery.com>

// Bug 531: We ICEd trying to give a parse error.

struct X
{
  bool operator (const X &) const;  // ERROR - parse error
};
