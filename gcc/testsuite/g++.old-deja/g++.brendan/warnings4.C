// { dg-do assemble  }
// GROUPS passed warnings
int
main ()
{
  register int x;
  int * foo = &x; // in C++ it's perfectly legal to do this

  return 0;
}
