// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ wasn't listing candidates for a failed conversion.

void f(int, double);		// ERROR - candidate
void f(double, int);		// ERROR - candidate
void f(int);			// ERROR - candidate

int
main ()
{
  void (*ptr)(int, int);
  
  ptr = &f;			// ERROR - no match
}
