// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Jul 2000 <nathan@codesourcery.com>

// Bug 306
// binding a reference in for scope creates nameless objects. Make sure
// we don't try and inject them into scope, for ARM compatibility.

void foo ()
{
  for (const int &thing = 0;;)
    ;
}
