// Bug: func is treated as an overloaded function when it isn't.
// Build don't link:
// Special g++ Options: -ansi -pedantic-errors -w

int *func () { return 0; }

void
test ()
{
  *func;			// gets bogus error - improper overloading
}
