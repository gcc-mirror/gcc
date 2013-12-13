// { dg-do assemble  }
// Testcase for simple overloading resolution.

int foo ();			// { dg-message "" } 
void foo ();			// { dg-error "" } disallowed overload
