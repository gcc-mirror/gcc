// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Mar 1999 <nathan@acm.org>

// Determine that function style casts are groked

void fn()
{
  +char(5);
  +short(5);
  +int(5);
  +long(5);
  +signed(5);
  +unsigned(5);
}
