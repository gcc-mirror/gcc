// PRMS Id: 4892
// Bug: COND_EXPRs, MODIFY_EXPRs and COMPOUND_EXPRs aren't properly recognized
// as lvalues.
// Build don't link:

extern int foo;
int& f (int& a, int& b)
{
  return (foo ? a : b);		// gets bogus error - 
}

int& g (int& a)
{
  return (a = 0);		// gets bogus error - 
}

int& h (int& a, int& b)
{
  return (a = 1, b);		// gets bogus error - 
}
