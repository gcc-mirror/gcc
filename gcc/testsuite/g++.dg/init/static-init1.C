// Related to the patch for 38880.
// Make sure we don't think we can initialize a at compile time.

char c;
short a[] = { (short)((int)&c + (int)&c) };
