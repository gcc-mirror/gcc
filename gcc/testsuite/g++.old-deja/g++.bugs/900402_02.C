// g++ 1.37.1 bug 900402_02

// g++ fails to correctly flag all attempts to construct an array type
// of zero length as errors.

// keywords: arrays, array bound, zero length

typedef int array_type[0];		// ERROR - gets warning only

int array_object_1[0];			// ERROR - gets warning only

void function_0 (int formal_array[0])
{					// ERROR - gets warning only
}

void function_2 ()
{
  int local_object_array_0[0];		// ERROR - gets warning only
}

int main () { return 0; }
