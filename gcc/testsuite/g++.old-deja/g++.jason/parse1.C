// Bug: g++ parses the declaration of r as a function declaration.
// Build don't link:

void foo (int i)
{
  int &r (i);
  r = 1;			// gets bogus error - 
}
