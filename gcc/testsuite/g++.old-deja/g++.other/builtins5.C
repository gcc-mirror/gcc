// { dg-do assemble  }
// Test that built-in functions aren't recognized without a prototype.
// Origin: Roger Sayle  Mar 20, 2002
// Copyright (C) 2002 Free Software Foundation.

int
foo ()
{
  return (int) ::strlen ("foo"); // { dg-error "" } undeclared
}

int
bar ()
{
  return (int) std::strlen ("bar"); // { dg-error "" } undeclared
}
