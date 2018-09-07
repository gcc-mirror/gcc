// PR c++/87152
// { dg-do run }
// { dg-options "-std=c++2a" }

struct A { int i; long long j; } a[64];

template<typename>
void foo ()
{
  A b = { 1, 2 };
  for (auto & [ u, v ] : a)
    {
      u = 2;
      v = 3;
    }

  for (auto [x, y] = b; auto [ u, v ] : a)
    if (y + u != x + v)
      __builtin_abort ();

  for (auto [x, y] = b; auto & [ u, v ] : a)
    if (y + u != x + v)
      __builtin_abort ();
}

int
main ()
{
  foo<int>();
}
