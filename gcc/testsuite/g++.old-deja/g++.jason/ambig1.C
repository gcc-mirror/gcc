// Testcase for ambiguity between functional cast and abstract declarator.
// This ambiguity accounts for 6 of the r/r conflicts.
// Special g++ Options: -pedantic-errors

int i = sizeof (int ());	// ERROR - sizeof applied to fn type
int j = sizeof (int () + 1);
