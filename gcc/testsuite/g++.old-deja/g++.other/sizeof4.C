// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sep 1999 <nathan@acm.org>

// C++ does not decay lvalues into rvalues until as late as possible. This
// means things like the rhs of a comma operator mustn't decay. This will make
// a difference if it is an array or function.

struct S;
struct T {int m;};
extern S s;  // an incomplete
extern S arys[20];  // an incomplete array
extern T aryt[];    // an incomplete array;

void fn () {}

int main (int argc, char **argv)
{
  sizeof (s);           // ERROR - incomplete
  sizeof (0, s);        // ERROR - incomplete
  sizeof (argc ? s : s); // ERROR - incomplete

  sizeof (arys);        // ERROR - incomplete
  sizeof (0, arys);     // ERROR - incomplete
  sizeof (argc ? arys : arys); // ERROR - incomplete

  sizeof (aryt);        // ERROR - incomplete
  sizeof (0, aryt);     // ERROR - incomplete
  sizeof (argc ? aryt : aryt); // ERROR - incomplete
  
  sizeof (fn);            // ERROR - cannot take size of function
  sizeof (0, fn);         // ERROR - cannot take size of function
  sizeof (argc ? fn : fn); // ERROR - cannot take size of function
  
  sizeof (&fn);       // ok
  sizeof (0, &fn);    // ok
  sizeof (argc ? &fn : &fn); // ok
  
  return 0;
}
