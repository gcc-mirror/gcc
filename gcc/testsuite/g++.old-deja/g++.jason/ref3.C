// PRMS Id: ????
// Build don't link:

void f (const int& i)
{
  &(int&)i;			// gets bogus error - references ARE lvalues
}
