// Build don't link:
// Test that built-in functions aren't recognized without a prototype.
// Origin: Roger Sayle  Mar 20, 2002
// Copyright (C) 2002 Free Software Foundation.

int
foo ()
{
  return (int) ::strlen ("foo"); // ERROR - undeclared
}

int
bar ()
{
  return (int) std::strlen ("bar"); // ERROR - undeclared
}
