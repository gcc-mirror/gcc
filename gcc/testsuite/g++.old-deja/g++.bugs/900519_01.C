// { dg-do assemble  }
// g++ 1.37.1 bug 900519_01

// g++ fails to flag errors for some attempts to declare or define non-member
// type conversion operators.

// cfront 2.0 passes this test.

// keywords: user-defined type conversion operator, non-member

extern operator int ();		// { dg-error "" } 

extern operator int () {	// { dg-error "" } 
  return 0;
}

int main () { return 0; }
