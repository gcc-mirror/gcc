// P0784R7
// { dg-do compile { target c++2a } }

struct S
{
  constexpr S () : r (4), s (3) { --r; s -= 2; }
  constexpr ~S () { if (s == 1) s = 0; else asm (""); if (s == 0 && r == 3) r = 0; else asm (""); }
  int r, s;
};
struct T : public S
{
  constexpr T () : t (2) {}
  int t;
  S u;
};
struct U : public S
{
  constexpr U (int x) : u (x) {}
  constexpr ~U () = default;
  int u;
  S v;
};

constexpr S a;
constexpr T b;
constexpr U c = 3;
static_assert (a.s == 1 && a.r == 3);
static_assert (b.s == 1 && b.r == 3 && b.t == 2 && b.u.s == 1 && b.u.r == 3);
static_assert (c.s == 1 && c.r == 3 && c.u == 3 && c.v.s == 1 && c.v.r == 3);

void
foo ()
{
  static constexpr S d;
  static constexpr T e;
  static constexpr U f = 4;
  static_assert (d.s == 1 && d.r == 3);
  static_assert (e.s == 1 && e.r == 3 && e.t == 2 && e.u.s == 1 && e.u.r == 3);
  static_assert (f.s == 1 && f.r == 3 && f.u == 4 && f.v.s == 1 && f.v.r == 3);
  if (1)
    {
      constexpr S g;
      constexpr T h;
      constexpr U i = 5;
      static_assert (g.s == 1 && g.r == 3);
      static_assert (h.s == 1 && h.r == 3 && h.t == 2 && h.u.s == 1 && h.u.r == 3);
      static_assert (i.s == 1 && i.r == 3 && i.u == 5 && i.v.s == 1 && i.v.r == 3);
    }
}

constexpr bool
bar ()
{
  S j;
  T k;
  U l = 6;
  if (j.s != 1 || j.r != 3)
    return false;
  if (k.s != 1 || k.r != 3 || k.t != 2 || k.u.s != 1 || k.u.r != 3)
    return false;
  if (l.s != 1 || l.r != 3 || l.u != 6 || l.v.s != 1 || l.v.r != 3)
    return false;
  return true;
}

static_assert (bar ());
