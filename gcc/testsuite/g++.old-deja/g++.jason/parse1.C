// { dg-do assemble  }
// Bug: g++ parses the declaration of r as a function declaration.

void foo (int i)
{
  int &r (i);
  r = 1;			// { dg-bogus "" } 
}
