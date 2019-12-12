// PR c++/87398
// { dg-do compile }

struct A { virtual int foo (); };

int
bar (int x)
{
  A e[5][2];
  int f = e[4][x].foo ();
  return f;
}
