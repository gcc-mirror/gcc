// g++ 1.37.1 bug 900331_04

// g++ is unable to correctly parse declarations of formal parameters and
// local objects which have pointer-to-array types or reference-to-array
// types.

// Cfront 2.0 passes this test.

// keywords: syntax, arrays, pointers, references, local, formal

int array[10];

int (*global_array_ptr)[10] = &array;
int (&global_array_ref)[10] = array;

void function0 (int (*formal_array_ptr)[10]) {	// gets bogus errors
}

void function1 (int (&formal_array_ref)[10]) {	// gets bogus errors
}

void function2 ()
{
  int (*local_array_ptr)[10] = &array;		// gets bogus errors
}

void function3 ()
{
  int (&local_array_ref)[10] = array;		// gets bogus error
}

int main () { return 0; }
