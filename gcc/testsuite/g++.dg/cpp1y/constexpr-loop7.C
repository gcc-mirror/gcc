// PR c++/87567
// { dg-do compile { target c++14 } }

constexpr bool always_false() { return false; }
int f() { return 1; }

constexpr int
fn1 ()
{
  while (always_false ())
    return f();
  return 0;
}

constexpr int
fn2 ()
{
  for (;always_false();)
    return f();
  return 0;
}
