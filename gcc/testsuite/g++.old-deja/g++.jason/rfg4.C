// Bug: f1 and f2 are treated as overloaded when they aren't.
// Build don't link:

int i;
void f1(double) { }
void f2(double) { }

void
test ()
{
  void (*ptr) (double);
  ptr = i ? f1 : f2;		// gets bogus error - improper overloading
}
