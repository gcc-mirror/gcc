// P0614R1
// { dg-do run }
// { dg-options "-std=c++2a" }

struct A { int i, j; };

int
main ()
{
  A a = { .i = 2, .j = 3 };
  int arr[] = { 1, 1, 1 };

  for (auto & [ x, y ] = a; auto z : arr)
    if (x + z != 3 || y + z != 4)
      __builtin_abort ();

  for (int d = 1; auto &z : arr)
    z += d;

  for (const auto [ x, y ] = a; auto z : arr)
    if (x + z != 4 || y + z != 5)
      __builtin_abort ();

  for (int d = 1; auto &z : arr)
    z += d;

  for (auto [ x, y ] = a; auto z : arr)
    if (x + z != 5 || y + z != 6)
      __builtin_abort ();
}
