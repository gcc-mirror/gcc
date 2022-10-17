// PR c++/70796
// { dg-do run { target c++11 } }
// { dg-options "-fstrong-eval-order" { target c++14_down } }

struct A
{
  int x = 0;
  A & operator ++ () { ++x; return *this; }
};
struct B
{
  A first, second;
  B (A x, A y) : first{x}, second{y} {}
};
struct C
{
  int first, second;
  C (int x, int y) : first{x}, second{y} {}
};
struct D
{
  int d;
  void foo (int x, D *y)
  {
    if (y != this + 1)
      __builtin_abort ();
    d = x;
  }
};
D d[2] = { { 1 }, { 2 } };

void
foo ()
{
  int i = 0;
  C p{++i, ++i};
  if (p.first != 1 || p.second != 2)
    __builtin_abort ();
}

void
bar ()
{
  int i = 0;
  C p{++i, ++i};
  if (p.first != 1 || p.second != 2)
    __builtin_abort ();
  int &j = i;
  C q{++j, ++j};
  if (q.first != 3 || q.second != 4)
    __builtin_abort ();
}

void
baz ()
{
  int i = 0;
  C p{(int &) ++i, (int &) ++i};
  if (p.first != 1 || p.second != 2)
    __builtin_abort ();
}

void
qux ()
{
  A i;
  B p{++i, ++i};
  if (p.first.x != 1 || p.second.x != 2)
    __builtin_abort ();
}

void
corge ()
{
  D *p = &d[0];
  p->foo (3, ++p);
  if (d[0].d != 3 || d[1].d != 2)
    __builtin_abort ();
}

int
main ()
{
  bar ();
  baz ();
  foo ();
  qux ();
  corge ();
}
