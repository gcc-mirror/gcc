// { dg-do run { target c++11 } }
// { dg-options "" }

struct A { int i; long long j; } a[64];
A b[32];

template <typename T>
void
foo (T &b)
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
  i = 0;
  for (auto &x : b)
    {
      x.i = i;
      x.j = 2 * i++;
    }
  for (auto & [ x, y ] : b)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      x -= 2;
      y -= 3;
    }
  i = 0;
  for (const auto [ u, v ] : b)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      if (u != i - 2 || v != 2 * i++ - 3)
	__builtin_abort ();
    }
  i = 0;
  for (auto [ x, y ] : b)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      x -= 4;
      y -= 5;
      if (x != i - 6 || y != 2 * i++ - 8)
	__builtin_abort ();
    }
  i = 0;
  for (const auto x : b)
    {
      if (x.i != i - 2 || x.j != 2 * i++ - 3)
	__builtin_abort ();
    }
}

int
main ()
{
  foo (b);
  for (int i = 0; i < 64; i++)
    {
      if (a[i].i != i + 2 || a[i].j != 2 * i + 3)
	__builtin_abort ();
      if (i >= 32)
	continue;
      if (b[i].i != i - 2 || b[i].j != 2 * i - 3)
	__builtin_abort ();
    }
}
