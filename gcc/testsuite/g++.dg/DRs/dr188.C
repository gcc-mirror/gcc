// DR 188
// { dg-do compile { target c++11 } }
// From [diff.expr]p5.

char arr[100];
// Yields 100 in C++ and sizeof(char*) in C.
static_assert (sizeof (0, arr) == 100, "");
