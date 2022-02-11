/* { dg-do compile } */
/* { dg-options "-O -fharden-compares -Wno-c++11-extensions" } */

enum E:bool
{ E0, E1 };

int x;

E
baz (E rtt)
{
  return rtt == E0 ? E1 : E0;
}

bool bar ();

void
foo (E)
{
  E a = x ? E1 : E0;
  if (bar ())
    if (bar ())
      {
	E b = baz (a);
	foo (b);
      }
}
