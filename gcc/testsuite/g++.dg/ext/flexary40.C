// PR c++/102295
// { dg-do run }
// { dg-options "" }

struct A { int a; int b[]; };
struct B { B (); int k; };
struct C { int l; B m[]; };

int x[4];
A c = { 42, { ++x[0], ++x[1], ++x[2], ++x[3] } };
A d = { 43, { 0, ++x[0], ++x[1], ++x[2], ++x[3] } };
A e = { 44, { ++x[0], ++x[1], ++x[2], 17 } };
A f = { 45 };
C n = { 50, { B (), B () } };
C o = { 51, {} };

int
main ()
{
  static A g = { 46, { ++x[0], ++x[1], ++x[2], ++x[3] } };
  static A h = { 47, { 0, ++x[0], ++x[1], ++x[2], ++x[3] } };
  static A i = { 48, { ++x[0], ++x[1], ++x[2], 18 } };
  static A j = { 49 };
  if (c.a != 42 || c.b[0] != 1 || c.b[1] != 1 || c.b[2] != 1 || c.b[3] != 1)
    __builtin_abort ();
  if (d.a != 43 || d.b[0] != 0 || d.b[1] != 2 || d.b[2] != 2 || d.b[3] != 2 || d.b[4] != 2)
    __builtin_abort ();
  if (e.a != 44 || e.b[0] != 3 || e.b[1] != 3 || e.b[2] != 3 || e.b[3] != 17)
    __builtin_abort ();
  if (f.a != 45)
    __builtin_abort ();
  if (g.a != 46 || g.b[0] != 4 || g.b[1] != 4 || g.b[2] != 4 || g.b[3] != 3)
    __builtin_abort ();
  if (h.a != 47 || h.b[0] != 0 || h.b[1] != 5 || h.b[2] != 5 || h.b[3] != 5 || h.b[4] != 4)
    __builtin_abort ();
  if (i.a != 48 || i.b[0] != 6 || i.b[1] != 6 || i.b[2] != 6 || i.b[3] != 18)
    __builtin_abort ();
  if (j.a != 49)
    __builtin_abort ();
  if (n.l != 50 || n.m[0].k != 42 || n.m[1].k != 42)
    __builtin_abort ();
  if (o.l != 51)
    __builtin_abort ();
  if (x[0] != 6 || x[1] != 6 || x[2] != 6 || x[3] != 4)
    __builtin_abort ();
}

B::B () : k (42)
{
}
