// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors -w" }
// Bug: func is treated as an overloaded function when it isn't.

int *func () { return 0; }

void
test ()
{
  *func;			// { dg-bogus "" } improper overloading
}
