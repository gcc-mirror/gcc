// g++ 1.37.1 bug 900324_05

// The following erroneous code causes g++ to segfault.

// Cfront 2.0 passes this test.

// keywords: segfault, arrays, references, assignment operator=

typedef int int_array[];

extern int_array int_array_object;

int_array &left = int_array_object;
int_array &right = int_array_object;

void function ()
{
  left = right;		// ERROR - causes segfault
}

int main () { return 0; }
