// Build don't link:
// 
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 1999 <nathan@acm.org>

// We cannot catch an incomplete type, or ptr to one

struct A; // ERROR - forward decl

void fn()
{
  try {}
  catch (A *p) {} // ERROR - undefined type
  try {}
  catch (A p) {}  // ERROR - undefined type
  try {}
  catch (void const *p) {}  // ok
}
