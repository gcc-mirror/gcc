// { dg-do assemble  }
// PRMS Id: 4892
// Bug: COND_EXPRs, MODIFY_EXPRs and COMPOUND_EXPRs aren't properly recognized
// as lvalues.

extern int foo;
int& f (int& a, int& b)
{
  return (foo ? a : b);		// { dg-bogus "" } 
}

int& g (int& a)
{
  return (a = 0);		// { dg-bogus "" } 
}

int& h (int& a, int& b)
{
  return (a = 1, b);		// { dg-bogus "" } 
}
