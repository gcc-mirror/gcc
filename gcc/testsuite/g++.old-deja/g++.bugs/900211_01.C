// g++ 1.36.1 bug 900211_01

// g++ issues only warnings for calls to previously undeclared functions,
// however such calls are actually errors.

// Cfront 2.0 passes this test.

// keywords: undeclared, functions
// Build don't link: 

void global_function_0 ()
{
  global_function_1 ();		/* ERROR - */
}

int main () { return 0; }
