// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed sizeof
// ARM $5.3.2

class bar;

int
main()
{
  // sizeof may not be applied to an undefined class
  int k = sizeof (bar);// { dg-error "11:invalid application of .sizeof. to incomplete type" } .*

  return 0;
}
