// Check that the type of an array is set correctly when flat initializers
// are used.

// { dg-do compile }

struct s { int a; int b; };
struct s x[] = { 1, 2, 3, 4 };
int y[sizeof (x) == 2 * sizeof (x[0])? 1 : -1];
