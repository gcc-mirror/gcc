// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed sizeof
// ARM $5.3.2

void f() { }

int
main()
{
  // sizeof may not be applied to a function
  int i = sizeof( f);// ERROR - .*

  return 0;
}
