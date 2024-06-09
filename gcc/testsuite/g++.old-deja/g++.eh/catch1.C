// { dg-do assemble  }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// 
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 1999 <nathan@acm.org>

// We cannot catch an incomplete type, or ptr to one

struct A; // { dg-message "" } forward decl

void fn()
{
  try {}
  catch (A *p) {} // { dg-error "" } undefined type
  try {}
  catch (A p) {}  // { dg-error "" } undefined type
  try {}
  catch (void const *p) {}  // ok
}
