// { dg-do run  }
// g++ 1.37.1 bug 900324_03

// g++ is unable to correctly parse declarations of non-global function-pointer
// variables and/or function-pointer formal parameters.

// Cfront 2.0 passes this test.

// keywords: syntax, function pointers, block local, formal

void (*p0)();				// OK

void function_0 ()
{
  void (*p1)();				// { dg-bogus "" } s
}

void function_1 (void (*p2)());		// { dg-bogus "" } s

void (*function_2 ()) ();		// OK

int main () { return 0; }
