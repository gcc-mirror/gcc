// { dg-do assemble  }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ wasn't listing candidates for a failed conversion.

void f(int, double);		// { dg-error "" } candidate
void f(double, int);		// { dg-error "" } candidate
void f(int);			// { dg-error "" } candidate

int
main ()
{
  void (*ptr)(int, int);
  
  ptr = &f;			// { dg-error "" } no match
}
