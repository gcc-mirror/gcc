// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed sizeof
// ARM $5.3.2

void f() { }

int
main()
{
  // sizeof may not be applied to a function
  int i = sizeof( f);// { dg-error "19:ISO C\\+\\+ forbids applying .sizeof." } .*

  return 0;
}
