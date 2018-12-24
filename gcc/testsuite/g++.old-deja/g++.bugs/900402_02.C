// { dg-do assemble  }
// g++ 1.37.1 bug 900402_02

// g++ fails to correctly flag all attempts to construct an array type
// of zero length as errors.

// keywords: arrays, array bound, zero length

typedef int array_type[0];		// { dg-error "24:ISO C\\+\\+ forbids zero-size array" }

int array_object_1[0];			// { dg-error "20:ISO C\\+\\+ forbids zero-size array" }

void function_0 (int formal_array[0])	// { dg-error "35:ISO C\\+\\+ forbids zero-size array" }
{
}

void function_2 ()
{
  int local_object_array_0[0];		// { dg-error "28:ISO C\\+\\+ forbids zero-size array" }
}

int main () { return 0; }
