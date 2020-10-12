// PR c++/96994
// { dg-do run { target c++20 } }

struct A { consteval A () { i = 1; } consteval A (int x) : i (x) {} int i = 0; };
struct B { constexpr B () { i = 1; } constexpr B (int x) : i (x) {} int i = 0; };
A const a;
constexpr A b;
B const c;
A const constinit d;
A const e = 2;
constexpr A f = 3;
B const g = 4;
A const constinit h = 5;
A i;
B j;
A k = 6;
B l = 7;
static_assert (b.i == 1 && f.i == 3);

int
main()
{
  if (a.i != 1 || c.i != 1 || d.i != 1 || e.i != 2 || g.i != 4 || h.i != 5
      || i.i != 1 || j.i != 1 || k.i != 6 || l.i != 7)
    __builtin_abort ();
}
