// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Nov 2000 <nathan@codesourcery.com>

// bug 706. We iced when meeting a decl with type error-mark-node

class bifstream;


int main()
{
  bifstream bifs;   // { dg-error "" } incomplete type
  if (!bifs)
    {
    }
  return 0;
}
