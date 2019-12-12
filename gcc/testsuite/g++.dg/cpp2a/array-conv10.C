// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++17 } }
// { dg-options "-Wpedantic" }

// The other direction: converting from int[] to int(&)[3] is forbidden.

extern int a[];
extern int (*b)[];
extern int (&c)[];
int (&y)[] = a;
int (&x)[3] = y; // { dg-error "cannot bind reference" }
int (&z)[3] = a; // { dg-error "cannot bind reference" }

void f(int (*)[3]);
void f2(int (&)[3]);

void
test ()
{
  f(b); // { dg-error "cannot convert" }
  f2(c); // { dg-error "cannot bind reference" }
}
