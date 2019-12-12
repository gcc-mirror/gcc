// PR c++/89507
// { dg-do compile }

unsigned char const n = 128;
int *p = new int[n];	// { dg-bogus "array exceeds maximum object size" }
