// General testcase for local classes.

int x;
void f ()
{
  static int s;
  int x;			// ERROR - referenced below
  extern int q();

  struct local {
    int g() { return x; }	// ERROR - automatic variable
    int h() { return s; }	// gets bogus error - local class
    int k() { return ::x; }	// OK
    int l() { return q(); }	// OK
    int m();			// OK - not defined
    static int foo;		// ERROR - static data member of local class
  };
}

local* p = 0;			// ERROR - no such type in scope
