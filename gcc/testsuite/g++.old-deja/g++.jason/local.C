// { dg-do assemble  }
// General testcase for local classes.

int x;
void f ()
{
  static int s;
  int x;			// { dg-error "" } referenced below
  extern int q();

  struct local {
    int g() { return x; }	// { dg-error "" } automatic variable
    int h() { return s; }	// { dg-bogus "" } local class
    int k() { return ::x; }	// OK
    int l() { return q(); }	// OK
    int m();			// OK - not defined
    static int foo;		// { dg-error "" } static data member of local class
  };
}

local* p = 0;			// { dg-error "" } no such type in scope
