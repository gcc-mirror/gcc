// PR c++/93529
// P1009: Array size deduction in new-expressions
// { dg-do compile { target c++11 } }

// Test error cases.
int *p = new double[] = { 1, 2, 3}; // { dg-error "invalid use of array with unspecified bounds" }
int *p2 = new double[] = (1, 2, 3); // { dg-error "invalid use of array with unspecified bounds" }
struct Aggr { int a; int b; int c; };
Aggr *p3 = new Aggr[]( 1, 2, 3 ); // { dg-error "could not convert|parenthesized initializer" }
char *p4 = new char[]("foo", "a"); // { dg-error "invalid conversion|parenthesized initializer" }

template<typename... T>
int *fn(T... t)
{
  return new int[]{t...}; // { dg-error "invalid conversion" }
}

void
g ()
{
  int *p = fn ("a");
}
