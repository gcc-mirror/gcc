// g++ 1.36.1 bug 900208_04

// The Cfront 2.0 reference manual (5.3.3) says "This type must be an
// object type; functions cannot be allocated this way...".

// g++ fails to detect (at compile time) cases where an attempt is made to
// deallocate a function using delete.

// Cfront 2.0 passes this test.

// keywords: operator delete, function types

typedef void (func_type) ();

void global_function_1 (func_type *p)
{
  delete p;		// ERROR - caught by Cfront 2.0 but not by g++ 1.36.1
}

int main () { return 0; }
