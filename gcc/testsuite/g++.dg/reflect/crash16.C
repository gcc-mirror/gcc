// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype (^^int);

struct A { int x, y; };
struct B { info x, y; };
struct C { int x; info y; };

void
foo ()
{
  constexpr A a { 3, 4 };
  constexpr auto [ax, ay] = a;
  static_assert (ax == 3);
  static_assert (ay == 4);

  constexpr B b { ^^::, ^^int };
  constexpr auto [bx, by] = b;
  static_assert (b.x == ^^::);
  static_assert (b.y == ^^int);
  static_assert (bx == ^^::);
  static_assert (by == ^^int);

  constexpr C c { 4, ^^:: };
  constexpr auto [cx, cy] = c;
  static_assert (c.x == 4);
  static_assert (c.y == ^^::);
  static_assert (cx == 4);
  static_assert (cy == ^^::);

  constexpr int d[] { 6, 7 };
  constexpr auto [dx, dy] = d;
  static_assert (dx == 6);
  static_assert (dy == 7);

  constexpr info e[] { ^^::, ^^int };
  constexpr auto [ex, ey] = e;
  static_assert (e[0] == ^^::);
  static_assert (e[1] == ^^int);
  static_assert (ex == ^^::);
  static_assert (ey == ^^int);
}
