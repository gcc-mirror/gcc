// { dg-do run { target c++11 } }
// { dg-options "" }

struct A { int i; long long j; } a[64];

int
main ()
{
  int i = 0;
  for (auto &x : a)
    {
      x.i = i;
      x.j = 2 * i++;
    }
  for (auto & [ x, y ] : a)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      x += 2;
      y += 3;
    }
  i = 0;
  for (const auto [ u, v ] : a)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      if (u != i + 2 || v != 2 * i++ + 3)
	__builtin_abort ();
    }
  i = 0;
  for (auto [ x, y ] : a)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      x += 4;
      y += 5;
      if (x != i + 6 || y != 2 * i++ + 8)
	__builtin_abort ();
    }
  i = 0;
  for (const auto x : a)
    {
      if (x.i != i + 2 || x.j != 2 * i++ + 3)
	__builtin_abort ();
    }
}
