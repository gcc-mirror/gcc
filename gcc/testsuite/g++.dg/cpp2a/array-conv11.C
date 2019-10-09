// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }
// { dg-options "-Wpedantic" }

// Test flexible array member.  Here we're binding int[] to int[].  This worked
// even before P0388R4.

typedef int T[];
extern T arr;
T &t1 = arr;

struct S {
  int i;
  int a[]; // { dg-warning "flexible array member" }
};

void f (int (&)[]);

void
test (S s)
{
  f (s.a);
}
