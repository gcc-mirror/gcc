// { dg-do assemble  }
// Testcase for simple overloading resolution.

int foo ();			// { dg-error "" } 
void foo ();			// { dg-error "" } disallowed overload
