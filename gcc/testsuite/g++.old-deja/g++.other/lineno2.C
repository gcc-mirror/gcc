// { dg-do assemble  }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ wasn't listing candidates for a failed conversion.

void f(int, double);		// { dg-message "" } candidate
void f(double, int);		// { dg-message "" } candidate
void f(int);			// { dg-message "" } candidate

int
main ()
{
  void (*ptr)(int, int);
  
  ptr = &f;			// { dg-error "" } no match
}
