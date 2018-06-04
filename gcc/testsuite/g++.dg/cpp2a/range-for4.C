// P0614R1
// { dg-do run }
// { dg-options "-std=c++2a" }

int
main ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (int i = 1; auto x : a)
    if (i++ != x)
      __builtin_abort ();

  int i;
  for (i = 1; auto x : a)
    if (i++ != x)
      __builtin_abort ();

  i = 0;
  for (i++; auto x : a)
    if (i != 1)
      __builtin_abort ();

  for (int s[] = { 1, 1, 1 }; auto x : s)
    if (x != 1)
      __builtin_abort ();
}
