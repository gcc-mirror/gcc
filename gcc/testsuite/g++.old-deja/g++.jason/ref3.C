// { dg-do assemble  }
// PRMS Id: ????

void f (const int& i)
{
  &(int&)i;			// { dg-bogus "" } references ARE lvalues
}
