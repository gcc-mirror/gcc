// g++ 1.37.1 bug 900321_04

// The following code causes g++ to segfault.

// Cfront 2.0 passes this test.

// keywords: segfault, object declaration, pointer, array, incomplete type

struct incomplete;

void function ()
{
  struct incomplete (*ptr)[];		// causes segfault
}

int main () { return 0; }
