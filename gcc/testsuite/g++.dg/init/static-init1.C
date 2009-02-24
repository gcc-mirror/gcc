// Related to the patch for 38880.
// Make sure we don't think we can initialize a at compile time.

char c;
short a[] = { (short)((__PTRDIFF_TYPE__)&c + (__PTRDIFF_TYPE__)&c) };
