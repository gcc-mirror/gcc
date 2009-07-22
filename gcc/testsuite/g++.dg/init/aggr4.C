struct A
{
  int i;
};

A a1 = { 1 };			// ok
A a2 = { a1 };			// { dg-error "cannot convert" }
