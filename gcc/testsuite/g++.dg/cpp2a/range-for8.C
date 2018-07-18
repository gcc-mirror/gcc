// P0614R1
// { dg-do run }
// { dg-options "-std=c++2a" }

struct A { int i; long long j; } a[64];

int
main ()
{
  for (int i = 0; auto &x : a)
    {
      x.i = i;
      x.j = 2 * i++;
    }
  for (auto & [ x, y ] : a)
    {
      x += 2;
      y += 3;
    }
  for (int i = 0; const auto [ u, v ] : a)
    {
      if (u != i + 2 || v != 2 * i++ + 3)
        __builtin_abort ();
    }
  for (int i = 0; auto [ x, y ] : a)
    {
      x += 4;
      y += 5;
      if (x != i + 6 || y != 2 * i++ + 8)
        __builtin_abort ();
    }
  for (int i = 0; const auto x : a)
    {
      if (x.i != i + 2 || x.j != 2 * i++ + 3)
        __builtin_abort ();
    }
}
