// Bug: func is treated as an overloaded function when it isn't.
// Build don't link:

int *func () { return 0; }

void
test ()
{
  int *(*p)() = *func;			// gets bogus error - improper overloading
}
