// P0614R1
// { dg-do run }
// { dg-options "-std=c++2a" }

int
main ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (int i = []{ return 3; }(); auto x : a)
    if (i != 3)
      __builtin_abort ();

  for (int i = ({ 3; }); auto x : a)
    if (i != 3)
      __builtin_abort ();
}
