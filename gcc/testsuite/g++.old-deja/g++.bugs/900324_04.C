// { dg-do assemble  }
// g++ 1.37.1 bug 900324_04

// g++ implements an extension which supports the copying of array objects.

// This extension is not described in the current C++ Reference Manual, and
// it is not implemented by cfront (2.0).

// g++ should generate errors for the use of this extension when -pedantic
// is used, however it does not.

// Cfront 2.0 passes this test.

// keywords: extension, pedantic, arrays, copy

typedef int int_array[20];

int_array ia1;
int_array ia2;

void function_0 ()
{
  ia1 = ia2;		// { dg-error "" } gnu extension
}

int main () { return 0; }
