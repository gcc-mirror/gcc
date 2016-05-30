// { dg-do assemble  }
// g++ 1.36.1 bug 900211_01

// g++ issues only warnings for calls to previously undeclared functions,
// however such calls are actually errors.

// Cfront 2.0 passes this test.

// keywords: undeclared, functions

void global_function_0 ()
{
  global_function_1 ();		/* { dg-error "3:'global_function_1' was not declared" } */
}

int main () { return 0; }
