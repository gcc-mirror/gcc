// PR c++/79572
// { dg-do run }
// { dg-options "-fsanitize=null -std=c++14" }
// { dg-output "reference binding to null pointer of type 'const int'" }

void
foo (const int &iref)
{
  if (&iref)
    __builtin_printf ("iref %d\n", iref);
  else
    __builtin_printf ("iref is NULL\n");
}

int
main ()
{
  foo (*((int*) __null));
}
