// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors -w" }
// Bug: f1 and f2 are treated as overloaded when they aren't.

int i;
void f1(double) { }
void f2(double) { }

void
test ()
{
  i ? f1 : f2;		// { dg-bogus "" } improper overloading
}
