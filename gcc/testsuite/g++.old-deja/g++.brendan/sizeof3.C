// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed sizeof
// ARM $5.3.2

class bar;

int
main()
{
  // sizeof may not be applied to an undefined class
  int k = sizeof (bar);// ERROR - .*

  return 0;
}
