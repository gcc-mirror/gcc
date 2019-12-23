// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// Testcase for ambiguity between functional cast and abstract declarator.
// This ambiguity accounts for 6 of the r/r conflicts.

int i = sizeof (int ());	// { dg-error "9:invalid application of .sizeof. to a function type" } sizeof applied to fn type
int j = sizeof (int () + 1);
